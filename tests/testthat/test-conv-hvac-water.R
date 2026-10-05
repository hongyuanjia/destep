# Binary source inputs isolate schedule identity, topology and units without
# borrowing any EnergyPlus reference model or real-model catalogue defaults.
hvac__water_fixture <- function() {
    con <- DBI::dbConnect(RSQLite::SQLite(), ':memory:')
    DBI::dbWriteTable(
        con,
        'AHU',
        data.frame(
            AHU_ID = 10L,
            EXT_PROPERTY = 1L,
            COOLING_COIL = 20L,
            REHEAT_TYPE = 0L
        )
    )
    DBI::dbWriteTable(
        con,
        'EXT_PROPERTY',
        data.frame(
            PROPERTY_ID = 1:4,
            NEXT_PROPERTY = c(2L, 3L, 4L, 0L),
            NAME = c(
                'AHU_WATER_TYPE',
                'AHU_TWO_PIPE_WATER_SCH',
                'AHU_FOUR_PIPE_COLD_WATER_SCH',
                'AHU_FOUR_PIPE_HOT_WATER_SCH'
            ),
            TYPE = 0L,
            DATA_LONG = c(1L, 50L, 51L, 52L),
            DATA_DOUBLE = 0
        )
    )
    DBI::dbWriteTable(
        con,
        'SCHEDULE_YEAR',
        data.frame(
            SCHEDULE_ID = 50:52,
            NAME = c('Shared Water', 'Cold Water', 'Hot Water')
        )
    )
    DBI::dbExecute(con, 'ALTER TABLE SCHEDULE_YEAR ADD COLUMN DATA BLOB')
    for (id in 50:52) {
        hours <- if (id == 50L) {
            rep(c(7, 60), length.out = 8760L)
        } else {
            rep(if (id == 51L) 7 else 60, 8760L)
        }
        DBI::dbExecute(
            con,
            'UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=?',
            params = list(
                list(writeBin(hours, raw(), size = 8L, endian = 'little')),
                id
            )
        )
    }
    con
}

test_that('two- and four-pipe water source identities stay separate from design points', {
    con <- hvac__water_fixture()
    on.exit(DBI::dbDisconnect(con))
    water <- hvac__water_boundary_source(con, 10L)
    expect_identical(water$role, c('cooling', 'heating'))
    expect_equal(water$schedule_id, c(50L, 50L))
    expect_equal(water$minimum_c, c(7, 7))
    expect_equal(water$maximum_c, c(60, 60))
    DBI::dbExecute(
        con,
        'UPDATE EXT_PROPERTY SET DATA_LONG=2 WHERE PROPERTY_ID=1'
    )
    water <- hvac__water_boundary_source(con, 10L)
    expect_equal(water$schedule_id, c(51L, 52L))
    expect_equal(water$minimum_c, c(7, 60))
    expect_equal(water$maximum_c, c(7, 60))
    DBI::dbExecute(
        con,
        'UPDATE EXT_PROPERTY SET DATA_LONG=999 WHERE PROPERTY_ID=3'
    )
    expect_error(
        hvac__water_boundary_source(con, 10L),
        class = 'destep_unresolved_hvac_water_boundary'
    )
})

test_that('invalid water input and absent main coil cannot become synthetic plant', {
    con <- hvac__water_fixture()
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, 'UPDATE AHU SET COOLING_COIL=0')
    expect_error(
        hvac__water_boundary_source(con, 10L),
        class = 'destep_unsupported_hvac_water_coil'
    )
    DBI::dbExecute(con, 'UPDATE AHU SET COOLING_COIL=20, REHEAT_TYPE=1')
    expect_error(
        hvac__water_boundary_source(con, 10L),
        class = 'destep_unsupported_hvac_reheat_type'
    )
    DBI::dbExecute(con, 'UPDATE AHU SET REHEAT_TYPE=0')
    DBI::dbExecute(
        con,
        'UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=50',
        params = list(list(raw(8)))
    )
    expect_error(
        hvac__water_boundary_source(con, 10L),
        class = 'destep_unresolved_hvac_water_boundary'
    )
    expect_error(
        hvac__boundary_options(con, list(chiller_nominal_cop = 4)),
        class = 'destep_conflicting_hvac_equipment_option'
    )
    expect_error(
        hvac__boundary_options(con, list(heating_coil_type = 'Electric')),
        class = 'destep_conflicting_hvac_equipment_option'
    )
})

test_that('missing target parameters receive disclosed per-system defaults', {
    source <- list(system = data.table::data.table(ac_system_id = 1L))
    cav <- hvac__boundary_system_options(source, 'multizone_cav')
    vav <- hvac__boundary_system_options(
        source,
        'multizone_vav',
        list(supply_fan_delta_pressure_pa = 800)
    )
    expect_equal(cav$supply_fan_delta_pressure_pa, 600)
    expect_equal(cav$return_fan_delta_pressure_pa, 300)
    expect_equal(vav$supply_fan_delta_pressure_pa, 800)
    expect_equal(vav$return_fan_delta_pressure_pa, 500)
    expect_identical(cav$preheat_coil_type, 'None')
    expect_identical(cav$heating_coil_type, 'HotWater')
    expect_equal(cav$zone_exhaust_fan_pressure_rise_pa, 0)
    expect_false(any(
        c('chiller_type', 'tower_type', 'boiler_type') %in% names(cav)
    ))
    expect_error(hvac__boundary_system_options(
        source,
        'multizone_cav',
        list(typo = 1)
    ))
})
# Supply-water temperature, not month or a guessed outdoor threshold, selects
# the compatible equivalent stage. Ambiguous ranges stay explicit failures.
test_that("two-pipe stage availability follows source temperature bounds", {
    con <- hvac__water_fixture()
    on.exit(DBI::dbDisconnect(con))
    DBI::dbWriteTable(
        con,
        "AC_SYS",
        data.frame(AC_SYS_ID = 1L, SUPPLY_T_MIN = 53L, SUPPLY_T_MAX = 54L)
    )
    for (id in 53:54) {
        DBI::dbExecute(
            con,
            "INSERT INTO SCHEDULE_YEAR VALUES(?,?,?)",
            params = list(
                id,
                paste("Air", id),
                list(writeBin(
                    rep(if (id == 53L) 14 else 32, 8760L),
                    raw(),
                    endian = "little"
                ))
            )
        )
    }
    water <- hvac__water_boundary_source(con, 10L)
    phase <- hvac__two_pipe_phase(con, 1L, water)
    expect_identical(phase$cooling, rep(c(1L, 0L), 4380L))
    expect_identical(phase$heating, 1L - phase$cooling)
    for (temperature in c(14, 20, 32)) {
        DBI::dbExecute(
            con,
            "UPDATE SCHEDULE_YEAR SET DATA=? WHERE SCHEDULE_ID=50",
            params = list(list(writeBin(
                rep(temperature, 8760L),
                raw(),
                endian = "little"
            )))
        )
        expect_error(
            hvac__two_pipe_phase(con, 1L, water),
            class = "destep_unsupported_hvac_water_changeover"
        )
    }
    data.table::set(water, NULL, "water_type", 2L)
    expect_null(hvac__two_pipe_phase(con, 1L, water))
})

# Network ownership determines support, independently of unused catalogue rows.
test_that("fan network restrictions identify only referenced systems", {
    con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
    on.exit(DBI::dbDisconnect(con))
    DBI::dbWriteTable(
        con,
        "AHU",
        data.frame(
            AHU_ID = c(10L, 20L, 30L, 40L),
            OF_AC_SYS = c(1L, 2L, 3L, NA_integer_),
            AHURES = c(0L, 200L, 300L, 400L)
        )
    )
    expect_invisible(hvac__assert_supported_fan_networks(con, integer()))
    expect_invisible(hvac__assert_supported_fan_networks(con, 1L))
    expect_error(
        hvac__assert_supported_fan_networks(con, c(1L, 2L)),
        "AC_SYS=2, AHU=20, DUCTNET=200",
        class = "destep_unsupported_hvac_fan_mapping"
    )
    expect_error(
        hvac__assert_supported_fan_networks(con, c(2L, 3L)),
        "AC_SYS=2, AHU=20, DUCTNET=200; AC_SYS=3, AHU=30, DUCTNET=300"
    )
    DBI::dbExecute(con, "DELETE FROM AHU")
    expect_invisible(hvac__assert_supported_fan_networks(con, 1L))
})

# Decode the emitted availability schedules independently, including frequent
# day changes, and verify the same schedules control each coil and plant loop.
test_that("batched water availability preserves hours and component references", {
    ep <- eplusr::empty_idf("9.1")
    # Populate required fields so the fixture exercises normal IDF validation.
    ep$add(`Schedule:Constant` = list(name = "Always On", hourly_value = 1))
    for (role in c("Chilled", "Hot")) {
        ep$add(
            `Pipe:Adiabatic` = list(
                name = paste(role, "Pipe"),
                inlet_node_name = paste(role, "Pipe In"),
                outlet_node_name = paste(role, "Pipe Out")
            ),
            Branch = list(
                name = paste(role, "Branch"),
                component_1_object_type = "Pipe:Adiabatic",
                component_1_name = paste(role, "Pipe"),
                component_1_inlet_node_name = paste(role, "Pipe In"),
                component_1_outlet_node_name = paste(role, "Pipe Out")
            ),
            PlantEquipmentList = list(
                name = paste(role, "Equipment"),
                equipment_1_object_type = "DistrictHeating",
                equipment_1_name = paste(role, "Source")
            ),
            DistrictHeating = list(
                name = paste(role, "Source"),
                hot_water_inlet_node_name = paste(role, "Source In"),
                hot_water_outlet_node_name = paste(role, "Source Out"),
                nominal_capacity = "Autosize"
            ),
            `PlantEquipmentOperation:Uncontrolled` = list(
                name = paste(role, "Uncontrolled"),
                equipment_list_name = paste(role, "Equipment")
            ),
            PlantEquipmentOperationSchemes = list(
                name = paste(role, "Operations"),
                control_scheme_1_object_type = "PlantEquipmentOperation:Uncontrolled",
                control_scheme_1_name = paste(role, "Uncontrolled"),
                control_scheme_1_schedule_name = "Always On"
            ),
            BranchList = list(
                name = paste(role, "Plant Branches"),
                branch_1_name = paste(role, "Branch")
            ),
            BranchList = list(
                name = paste(role, "Demand Branches"),
                branch_1_name = paste(role, "Branch")
            )
        )
        ep$add(
            PlantLoop = list(
                name = paste("DeST", role, "Water Loop", role, "Water Loop"),
                plant_equipment_operation_scheme_name = paste(
                    role,
                    "Operations"
                ),
                loop_temperature_setpoint_node_name = paste(role, "Setpoint"),
                maximum_loop_temperature = 100,
                minimum_loop_temperature = 0,
                maximum_loop_flow_rate = "Autosize",
                plant_side_inlet_node_name = paste(role, "Plant Inlet"),
                plant_side_outlet_node_name = paste(role, "Plant Outlet"),
                plant_side_branch_list_name = paste(role, "Plant Branches"),
                demand_side_inlet_node_name = paste(role, "Demand Inlet"),
                demand_side_outlet_node_name = paste(role, "Demand Outlet"),
                demand_side_branch_list_name = paste(role, "Demand Branches")
            )
        )
    }
    for (role in c("Cooling", "Heating")) {
        coil <- list(
            name = paste("DeST AC_SYS 1", role, "Coil"),
            water_inlet_node_name = paste(role, "Water Inlet"),
            water_outlet_node_name = paste(role, "Water Outlet"),
            air_inlet_node_name = paste(role, "Air Inlet"),
            air_outlet_node_name = paste(role, "Air Outlet")
        )
        do.call(
            ep$add,
            stats::setNames(list(coil), paste0("Coil:", role, ":Water"))
        )
    }
    cooling <- rep(rep(c(0L, 1L), length.out = 365L), each = 24L)
    phases <- list(cooling = cooling, heating = 1L - cooling)
    expect_invisible(hvac__apply_water_phase(
        ep,
        list(list(system_id = 1L, water_phase = phases))
    ))
    for (role in c("cooling", "heating")) {
        label <- if (role == "cooling") "Cooling" else "Heating"
        name <- paste("DeST Two Pipe", label, "Availability")
        fields <- ep$object(name)$to_table()$value
        expect_equal(
            destep_test_expand_compact(fields),
            as.double(phases[[role]])
        )
        coil <- ep$object(paste("DeST AC_SYS 1", label, "Coil"))
        expect_identical(
            unname(unlist(coil$value("availability_schedule_name"))),
            name
        )
        water <- if (role == "cooling") "Chilled" else "Hot"
        loop <- ep$object(paste(
            "DeST",
            water,
            "Water Loop",
            water,
            "Water Loop"
        ))
        manager <- ep$object(paste(name, "Manager"))
        expect_identical(
            unname(unlist(loop$value("availability_manager_list_name"))),
            paste(name, "Manager List")
        )
        expect_identical(unname(unlist(manager$value("schedule_name"))), name)
    }
})
