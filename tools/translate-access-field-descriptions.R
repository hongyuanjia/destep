#!/usr/bin/env Rscript

source_fields <- file.path("inst", "schema", "fields.tsv")
source_fields_cn <- file.path("inst", "schema", "fields_cn.tsv")

write_schema_tsv <- function(x, file) {
    x[] <- lapply(x, function(column) {
        column[is.na(column)] <- ""
        as.character(column)
    })

    has_embedded_delimiter <- vapply(x, function(column) {
        any(grepl("[\t\r\n]", column))
    }, logical(1L))
    if (any(has_embedded_delimiter)) {
        stop("TSV fields must not contain tabs or line breaks.", call. = FALSE)
    }

    escaped <- x
    escaped[] <- lapply(escaped, function(column) {
        column[!nzchar(column)] <- "\"\""
        column
    })
    lines <- c(
        paste(names(escaped), collapse = "\t"),
        do.call(paste, c(escaped, sep = "\t"))
    )

    con <- file(file, open = "w", encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(lines, con, useBytes = TRUE)
}

if (!file.exists(source_fields) || !file.exists(source_fields_cn)) {
    stop("Run this script from the package root after generating fields_cn.tsv.", call. = FALSE)
}

fields <- utils::read.delim(
    source_fields,
    sep = "\t",
    quote = "",
    comment.char = "",
    na.strings = character(),
    colClasses = "character",
    check.names = FALSE
)
fields_cn <- utils::read.delim(
    source_fields_cn,
    sep = "\t",
    quote = "",
    comment.char = "",
    na.strings = character(),
    colClasses = "character",
    check.names = FALSE
)
fields[] <- lapply(fields, function(column) {
    column[column == "\"\""] <- ""
    column
})
fields_cn[] <- lapply(fields_cn, function(column) {
    column[column == "\"\""] <- ""
    column
})

translation <- c(
    "0 全空气定风量， 1 全空气变风量，2 风机盘管，3 独立空调,4 干式风机盘管,5 独立除湿新风机组,6 变制冷剂流量空调系统" = "Air-conditioning system type: 0 constant-air-volume all-air system; 1 variable-air-volume all-air system; 2 fan coil unit; 3 unitary air conditioner; 4 dry fan coil unit; 5 dedicated dehumidifying outdoor-air unit; 6 variable refrigerant flow system.",
    "0 定新风比（用最小新风比），1：以新风比范围限制新风 2：定新风量(此时用最小新风量），3：以新风量范围定限制新风" = "Outdoor-air control type: 0 fixed outdoor-air ratio using the minimum ratio; 1 limited by an outdoor-air ratio range; 2 fixed outdoor-air flow using the minimum flow; 3 limited by an outdoor-air flow range.",
    "0: 普通表面 1: 外表面 2: 与大地相接的表面 3：遮阳板的表面,4 水平朝上的内表面,5 水平朝下的内表面,6 需要计算的指向中庭的垂直内表面,7 不需要计算的背向中庭、指向外界的天窗外表面,8 需要计算的指向中庭的水平朝上内表面,9 需要计算的指向中庭的水平朝下内表面,9 虚拟构件的表面" = "Surface state code: 0 ordinary surface; 1 exterior surface; 2 ground-contact surface; 3 shading surface; 4 upward-facing horizontal interior surface; 5 downward-facing horizontal interior surface; 6 atrium-facing vertical interior surface to be calculated; 7 skylight exterior surface facing outdoors away from the atrium and not calculated; 8 atrium-facing upward horizontal interior surface to be calculated; 9 atrium-facing downward horizontal interior surface to be calculated or virtual component surface.",
    "0：一般表冷盘管，1：带旁通的表冷盘管" = "Cooling coil type: 0 ordinary cooling coil; 1 cooling coil with bypass.",
    "0~8759小时" = "Hour index from 0 to 8759.",
    "0无水管，1 只有热水（两管制），2 只有冷水（两管制），3 冷热水（四管制）， 4 两管制（冷热切换）" = "Water loop type: 0 none; 1 hot water only, two-pipe; 2 chilled water only, two-pipe; 3 hot and chilled water, four-pipe; 4 two-pipe changeover.",
    "1:ratio,2:on_off,3:Int,4:float,5:double,但DATA都按double存" = "Schedule type code: 1 ratio; 2 on/off; 3 integer; 4 float; 5 double; DATA is stored as double in all cases.",
    "1:string 2:long 3:double 4:float 5:short 6:type 其余值错" = "Default value type code: 1 string; 2 long; 3 double; 4 float; 5 short; 6 type; other values are invalid.",
    "HVAC热扰的ID，这个值也必须是全局唯一的。" = "HVAC heat-gain ID; this value must also be globally unique.",
    "ID, 同时为图上的句柄" = "ID, also the drawing handle.",
    "ID,同时是图上的HANDLE" = "ID, also the drawing handle.",
    "K值" = "K value.",
    "SC值" = "SC value.",
    "X坐标（单位：m）" = "X coordinate in m.",
    "Y坐标（单位：m）" = "Y coordinate in m.",
    "Z坐标（单位：m）" = "Z coordinate in m.",
    "中平面" = "Middle plane.",
    "中平面的ID" = "Middle plane ID.",
    "中文名" = "Chinese name.",
    "为0时表示空气" = "A value of 0 indicates air.",
    "为0表示没有这个设备，1－蒸气加湿，2－电加湿，3－喷水加湿" = "Humidifier type: 0 no device; 1 steam humidification; 2 electric humidification; 3 spray humidification.",
    "为0表示没有这个设备，不为0时表示有这一设备，为正时则为从设备数据库中选择的设备的ID，为负时表示这一设备未被选择。" = "Equipment reference: 0 means absent; nonzero means present; positive values are selected equipment IDs from the equipment database; negative values mean the equipment has not been selected.",
    "主管连接点位置" = "Main-pipe connection point location.",
    "主索引" = "Primary index.",
    "主索引,门编号" = "Primary index and door number.",
    "乘以温差和面积就是换热量，温差是天空与表面的温差" = "Coefficient that gives heat transfer when multiplied by area and the temperature difference between sky and surface.",
    "人员作息模式" = "Occupant schedule.",
    "人员热扰分配模式" = "Occupant heat-gain distribution mode.",
    "仅用于遮阳表面，标识该表面所属的遮阳，对应SHADING表" = "For shading surfaces only; identifies the owning shading object and references the SHADING table.",
    "作息模式" = "Schedule.",
    "倾角（单位：°）,与水平面的夹角。垂直表面为90°。凸台的侧面大于90°。" = "Tilt angle in degrees relative to the horizontal plane; vertical surfaces are 90 degrees, and protrusion side surfaces are greater than 90 degrees.",
    "做Stiffen刚性化处理时的期望值" = "Expected value used for Stiffen rigidification.",
    "最外围的loop_point的ID" = "ID of the outermost loop point.",
    "最多人数(人 或 人/平米）" = "Maximum number of people, as people or people per square meter.",
    "最大二次回风比" = "Maximum secondary return-air ratio.",
    "最大产热功率（单位：W或 W/M^2）" = "Maximum heat-gain power, in W or W/m2.",
    "最大换气次数(次/Hr)" = "Maximum air changes per hour.",
    "最大换湿系数" = "Maximum moisture-exchange coefficient.",
    "最大新风比" = "Maximum outdoor-air ratio.",
    "最大新风量" = "Maximum outdoor-air flow rate.",
    "最大显热交换系数" = "Maximum sensible heat-exchange coefficient.",
    "最大末端再热量（最小为0）" = "Maximum terminal reheat, with a minimum of 0.",
    "最大设备散湿（kg/hr 或 Kg/(Hr.m^2)）" = "Maximum equipment moisture generation, in kg/hr or kg/(hr.m2).",
    "最大送风温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID" = "Maximum supply-air temperature schedule ID, corresponding to SCHEDULE_YEAR.SCHEDULE_ID.",
    "最大铬牌功率（单位：W或 W/M^2）" = "Maximum nameplate power, in W or W/m2.",
    "最小二次回风比" = "Minimum secondary return-air ratio.",
    "最小产热功率（单位：W或 W/M^2）" = "Minimum heat-gain power, in W or W/m2.",
    "最小换气次数(次/Hr)" = "Minimum air changes per hour.",
    "最小换湿系数" = "Minimum moisture-exchange coefficient.",
    "最小新风比" = "Minimum outdoor-air ratio.",
    "最小新风量" = "Minimum outdoor-air flow rate.",
    "最小新风量( M3/Hr)" = "Minimum outdoor-air flow rate, in m3/hr.",
    "最小显热交换系数" = "Minimum sensible heat-exchange coefficient.",
    "最小设备散湿（kg/hr 或 Kg/(Hr.m^2)）" = "Minimum equipment moisture generation, in kg/hr or kg/(hr.m2).",
    "最小送风温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID" = "Minimum supply-air temperature schedule ID, corresponding to SCHEDULE_YEAR.SCHEDULE_ID.",
    "最小铬牌功率（单位：W或 W/M^2）" = "Minimum nameplate power, in W or W/m2.",
    "最少人数(人 或 人/平米）" = "Minimum number of people, as people or people per square meter.",
    "几何参数 DIFFERENCE" = "Geometry parameter: DIFFERENCE.",
    "几何参数 HEIGHT" = "Geometry parameter: HEIGHT.",
    "几何参数 LEFT DIFFERENCE" = "Geometry parameter: LEFT DIFFERENCE.",
    "几何参数 LEFT HEIGHT" = "Geometry parameter: LEFT HEIGHT.",
    "几何参数 LEFT WIDTH" = "Geometry parameter: LEFT WIDTH.",
    "几何参数 RIGHT DIFFERENCE" = "Geometry parameter: RIGHT DIFFERENCE.",
    "几何参数 RIGHT HEIGHT" = "Geometry parameter: RIGHT HEIGHT.",
    "几何参数 RIGHT WIDTH" = "Geometry parameter: RIGHT WIDTH.",
    "几何参数WIDTH" = "Geometry parameter: WIDTH.",
    "几何形状编号,对应GEOMETRY表中的 GEOMETRY ID" = "Geometry identifier corresponding to GEOMETRY.GEOMETRY_ID.",
    "几何形状编号，对应GEOMETRY表中的GEOMETRY_ID" = "Geometry identifier corresponding to GEOMETRY.GEOMETRY_ID.",
    "凸窗面积修正系数" = "Bay-window area correction factor.",
    "分割墙体份数的判据" = "Criterion for the number of wall subdivisions.",
    "分配到周围立面的部分" = "Fraction distributed to surrounding facades.",
    "分配到地板的部分" = "Fraction distributed to the floor.",
    "分配到室内空气的部分" = "Fraction distributed to indoor air.",
    "分配到屋顶的部分" = "Fraction distributed to the roof.",
    "分配模式" = "Distribution mode.",
    "删除标记" = "Deletion flag.",
    "厚度(mm)" = "Thickness in mm.",
    "发射率" = "Emissivity.",
    "变送风状态标志.0: dest Decide; 1:user defined" = "Variable supply-air state flag: 0 decided by DeST; 1 user-defined.",
    "可见光反射率" = "Visible-light reflectance.",
    "可见光透过率" = "Visible-light transmittance.",
    "台站属性,0-15位(0-36000):时区经度(东经0-18000,西经18000-36000,含两位小数); 16位:当地太阳时标记; 17位:风速风向缺测标记;18位:地表温度缺测标记;19位:天空等效温度缺测标记;20位:逐时大气压力缺测标记" = "Station property bit field: bits 0-15 store time-zone longitude from 0 to 36000 with two decimals, east longitude 0-18000 and west longitude 18000-36000; bit 16 marks local solar time; bit 17 marks missing wind speed/direction; bit 18 marks missing ground surface temperature; bit 19 marks missing sky equivalent temperature; bit 20 marks missing hourly atmospheric pressure.",
    "名字" = "Name.",
    "名称" = "Name.",
    "含湿量(g/kg.dra)" = "Humidity ratio, in g/kg dry air.",
    "和COOLING_COIL不能同时存在于同一个AHU中。为0表示没有这个设备，不为0时表示有这一设备，为正时则为从设备数据库中选择的设备的ID，为负时表示这一设备未被选择。" = "Sprayer reference, mutually exclusive with COOLING_COIL in the same AHU: 0 means absent; nonzero means present; positive values are selected equipment IDs from the equipment database; negative values mean the equipment has not been selected.",
    "国别(中文名称)" = "Country, Chinese name.",
    "国别(英文名称)" = "Country, English name.",
    "国家" = "Country.",
    "图中DNAHU插入点的X坐标" = "X coordinate of the DNAHU insertion point in the drawing.",
    "图中DNAHU插入点的Y坐标" = "Y coordinate of the DNAHU insertion point in the drawing.",
    "图中DNAHU插入点的Z坐标" = "Z coordinate of the DNAHU insertion point in the drawing.",
    "图中DPECompass(指北针）放置位置的X坐标" = "X coordinate of the DPECompass north-arrow location in the drawing.",
    "图中DPECompass(指北针）放置位置的Y坐标" = "Y coordinate of the DPECompass north-arrow location in the drawing.",
    "图中DPECompass(指北针）放置位置的Z坐标" = "Z coordinate of the DPECompass north-arrow location in the drawing.",
    "图中人员得热的X坐标" = "X coordinate of the occupant heat-gain object in the drawing.",
    "图中人员得热的Y坐标" = "Y coordinate of the occupant heat-gain object in the drawing.",
    "图中人员得热的Z坐标" = "Z coordinate of the occupant heat-gain object in the drawing.",
    "图中南向与X轴成的角度（°）" = "Angle in degrees between south direction and the X axis in the drawing.",
    "图中灯光得热的X坐标" = "X coordinate of the lighting heat-gain object in the drawing.",
    "图中灯光得热的Y坐标" = "Y coordinate of the lighting heat-gain object in the drawing.",
    "图中灯光得热的Z坐标" = "Z coordinate of the lighting heat-gain object in the drawing.",
    "图中表示该Ground的房间标志（如果有的话）的X坐标" = "X coordinate of the room marker representing this Ground in the drawing, if present.",
    "图中表示该Ground的房间标志（如果有的话）的Y坐标" = "Y coordinate of the room marker representing this Ground in the drawing, if present.",
    "图中表示该Ground的房间标志（如果有的话）的Z坐标" = "Z coordinate of the room marker representing this Ground in the drawing, if present.",
    "图中表示该Outside的房间标志（如果有的话）的X坐标" = "X coordinate of the room marker representing this Outside in the drawing, if present.",
    "图中表示该Outside的房间标志（如果有的话）的Y坐标" = "Y coordinate of the room marker representing this Outside in the drawing, if present.",
    "图中表示该Outside的房间标志（如果有的话）的Z坐标" = "Z coordinate of the room marker representing this Outside in the drawing, if present.",
    "图中设备得热放置位置的X坐标" = "X coordinate of the equipment heat-gain object in the drawing.",
    "图中设备得热放置位置的Y坐标" = "Y coordinate of the equipment heat-gain object in the drawing.",
    "图中设备得热放置位置的Z坐标" = "Z coordinate of the equipment heat-gain object in the drawing.",
    "图例" = "Legend.",
    "图片" = "Image.",
    "在Zip landa时的容忍度，相差不大于ZIP_TOLERANCE的landa被认为是相同的。" = "Tolerance used when zipping landa values; landa values differing by no more than ZIP_TOLERANCE are considered identical.",
    "在所属供热系统立管中的序号(从0开始)" = "Zero-based index within the owning heating-system riser.",
    "地表温度(℃)" = "Ground surface temperature in degrees C.",
    "地面反射率" = "Ground reflectance.",
    "城市(中文名称)" = "City, Chinese name.",
    "城市(英文名称)" = "City, English name.",
    "城市名称" = "City name.",
    "城市编号" = "City identifier.",
    "基线（房间划分依据线的终点）,对应POINT表中的ID" = "Baseline endpoint used for room division, corresponding to POINT.ID.",
    "基线（房间划分依据线的起点）,对应POINT表中的ID" = "Baseline start point used for room division, corresponding to POINT.ID.",
    "墙所在的楼层" = "Storey containing the wall.",
    "备注" = "Remark.",
    "外遮阳对散射辐射的遮阳系数" = "Diffuse-radiation shading coefficient of exterior shading.",
    "大气压力(Pa)" = "Atmospheric pressure in Pa.",
    "天空有效温度(K)" = "Effective sky temperature in K.",
    "太阳能反射率" = "Solar reflectance.",
    "太阳能透过率" = "Solar transmittance.",
    "太阳透过热扰的分配系数" = "Distribution coefficient for transmitted solar heat gain.",
    "如果值为DOUBLE，则此处有值" = "Value stored here when the value type is DOUBLE.",
    "如果值为FLOAT，则此处有值" = "Value stored here when the value type is FLOAT.",
    "如果值为INT，则此处有值" = "Value stored here when the value type is INT.",
    "如果值为LONG，则此处有值" = "Value stored here when the value type is LONG.",
    "如果值为字符串，则此处有值" = "Value stored here when the value type is string.",
    "字段名" = "Field name.",
    "定压比热(J/kg.K)" = "Specific heat at constant pressure in J/kg.K.",
    "室内机位置" = "Indoor-unit location.",
    "室内机类型编号" = "Indoor-unit type identifier.",
    "室外机位置" = "Outdoor-unit location.",
    "室外机类型编号" = "Outdoor-unit type identifier.",
    "家俱系数(几个房间的总的系数=各房间系数与体积的加权平均）" = "Furniture coefficient; the combined coefficient for multiple rooms is the volume-weighted average of each room coefficient.",
    "容积（单位：m3）" = "Volume in m3.",
    "密度(kg/m^3)" = "Density in kg/m3.",
    "对应OUSIDE表中的一条记录，新风来源" = "Outdoor-air source, corresponding to one record in the OUTSIDE table.",
    "对应OUSIDE表中的外温或GROUND表中的地温，如果为0，则无此项" = "Outside temperature in OUTSIDE or ground temperature in GROUND; 0 means absent.",
    "对应USER_DEF_DLL表中的用户自定义DLL的DLL_ID" = "User-defined DLL_ID corresponding to USER_DEF_DLL.",
    "对应地温数据表SYS_GROUND_DATA的ID" = "Identifier corresponding to SYS_GROUND_DATA.",
    "对应气象数据表SYS_CLIMATE_DATA的ID" = "Identifier corresponding to SYS_CLIMATE_DATA.",
    "对应空调箱阻力模型(DUCTNET)的ID" = "Identifier of the air-handling-unit resistance model, corresponding to DUCTNET.",
    "对应设备数据库中设备的ID" = "Equipment identifier in the equipment database.",
    "对流换热系数" = "Convective heat-transfer coefficient.",
    "导热热阻(m^2.K/W)" = "Thermal resistance in m2.K/W.",
    "导热系数" = "Thermal conductivity.",
    "导热系数(W/m.K)" = "Thermal conductivity in W/m.K.",
    "市场参考价格" = "Market reference price.",
    "常年大气压(Pa)" = "Annual typical atmospheric pressure in Pa.",
    "干球温度(℃)" = "Dry-bulb temperature in degrees C.",
    "开启作息模式编号(预留)" = "Opening schedule identifier, reserved.",
    "开启作息模式编号（预留）" = "Opening schedule identifier, reserved.",
    "引用标记" = "Reference flag.",
    "当前操作楼层的ID" = "Identifier of the currently active storey.",
    "房间与外界通风最大能力" = "Maximum ventilation capacity between the room and outdoors.",
    "房间类型(用于人员负荷的参考) 对应ROOM_TYPE_DATA表" = "Room type used as a reference for occupant loads, corresponding to ROOM_TYPE_DATA.",
    "房间类型名" = "Room type name.",
    "房间设定最低照度(Lx)" = "Room minimum illuminance setting in lx.",
    "房间设定温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID" = "Room temperature setpoint schedule ID, corresponding to SCHEDULE_YEAR.SCHEDULE_ID.",
    "房间设定温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID，空调设定温度上限" = "Room temperature setpoint schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID; upper air-conditioning setpoint temperature.",
    "房间设定温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID，空调设定温度下限" = "Room temperature setpoint schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID; lower air-conditioning setpoint temperature.",
    "房间设定湿度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID" = "Room humidity setpoint schedule ID, corresponding to SCHEDULE_YEAR.SCHEDULE_ID.",
    "房间设定湿度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID，空调设定湿度上限" = "Room humidity setpoint schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID; upper air-conditioning humidity setpoint.",
    "房间设定湿度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID，空调设定湿度下限" = "Room humidity setpoint schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID; lower air-conditioning humidity setpoint.",
    "所在序号" = "Sequence number within the owning object.",
    "所在建筑(没有所在楼层的概念，用所在建筑标识)" = "Owning building; used when there is no owning-storey concept.",
    "所在房间" = "Owning room.",
    "所在的墙的ID" = "Identifier of the owning wall.",
    "所在的楼层的ID，如果是在地板或天花板上时，指天花板所在的楼层的ID" = "Identifier of the owning storey; for floors or ceilings, this refers to the storey containing the ceiling.",
    "所属供热系统立管编号(对应HEATING_PIPE表)" = "Owning heating-system riser identifier, corresponding to HEATING_PIPE.",
    "所属室外机编号" = "Owning outdoor-unit identifier.",
    "所属建筑(方便结果浏览程序)" = "Owning building, used by the result browser.",
    "所属建筑的ID" = "Owning building ID.",
    "所属建筑的编号，与BUILDING表的BUILDING_ID关联" = "Owning building identifier, related to BUILDING.BUILDING_ID.",
    "所属房间组编号" = "Owning room-group identifier.",
    "所属楼层" = "Owning storey.",
    "所属楼层的ID" = "Owning storey ID.",
    "所属的geometry的ID" = "Owning geometry ID.",
    "所属的建筑" = "Owning building.",
    "所属的房间,可以是ROOM中的房间，也可以是OUTSIDE或GROUND表中的外界或大地" = "Owning room; can reference a ROOM record, an OUTSIDE boundary, or a GROUND boundary.",
    "所属的房间组" = "Owning room group.",
    "所属的楼层组" = "Owning storey group.",
    "所属空调系统的ID，为0时不属于任何系统。" = "Owning air-conditioning system ID; 0 means it does not belong to any system.",
    "所属系统的ID" = "Owning system ID.",
    "扩展属性(扩展属性编号,对应EXT_PROPERTY表)" = "Extended property reference, corresponding to EXT_PROPERTY.",
    "折射指数" = "Refractive index.",
    "拔海高度(m)" = "Elevation in m.",
    "换气次数对应的Schedule_id" = "Schedule ID corresponding to air changes.",
    "散热器换热系数修正系数A(K=A*dT^B)" = "Radiator heat-transfer coefficient correction factor A in K = A*dT^B.",
    "散热器换热系数修正系数B(K=A*dT^B)" = "Radiator heat-transfer coefficient correction factor B in K = A*dT^B.",
    "散热器流量全年变化作息(m^3/h)" = "Annual radiator flow schedule in m3/h.",
    "散热器热量分配模式（对应DIST_MODE表）" = "Radiator heat distribution mode, corresponding to DIST_MODE.",
    "散热器面积(m^2)" = "Radiator area in m2.",
    "数据" = "Data.",
    "旧的窗遮阳构件编号 对应SYS_SHADING中的ID，0表示没有遮阳。" = "Legacy window shading component identifier, corresponding to SYS_SHADING.ID; 0 means no shading.",
    "是否允许再热,0－没有设备，1－电加热，2－热水盘管" = "Reheat permission/type: 0 no device; 1 electric heating; 2 hot-water coil.",
    "是否按每平米计" = "Whether the value is per square meter.",
    "是否按每平米计，0按总量计，1按平米指标计，2按功能计(使用A+BF公式)" = "Calculation basis flag: 0 total value; 1 per-area index; 2 functional calculation using the A + B*F formula.",
    "是否用户指定" = "Whether user-defined.",
    "有否二次回风" = "Whether secondary return air is present.",
    "本房间的ID" = "Current room ID.",
    "材料中文名称" = "Material Chinese name.",
    "材料编号" = "Material identifier.",
    "材料英文名称" = "Material English name.",
    "材料表中门体材料的ID" = "Door-body material ID in the material table.",
    "构件号ID,由种类的不同，对应系统库中六个表中的ID，为0则使用缺省值。" = "Component ID; depending on the type, it corresponds to IDs in one of six system-library tables; 0 uses the default value.",
    "标准楼层的代表层数，MULTIPLE>1的楼层即为标准层" = "Number of represented floors for a typical storey; storeys with MULTIPLE > 1 are typical storeys.",
    "标志" = "Flag.",
    "标记" = "Flag.",
    "楼层" = "Storey.",
    "楼层号0为第一层，-1为地下一层" = "Storey number; 0 is the first floor and -1 is the first basement level.",
    "楼层是否可见" = "Whether the storey is visible.",
    "楼层高度(单位m)" = "Storey height in m.",
    "每人产湿量（Kg/hr）" = "Moisture generation per person in kg/hr.",
    "每人产热量（W）" = "Heat gain per person in W.",
    "每人所需最小新风量(m^3/h)" = "Minimum outdoor-air flow required per person in m3/h.",
    "气候类型" = "Climate type.",
    "气象台站ID，北京54511" = "Weather station ID; Beijing is 54511.",
    "气象数据的来源标记, 0-3位(0-15)来源: 0-内置, 1-DeST气象数据库, 2-公开的气象数据集, 3-用户输入, 4-15其它数据源; 4-7位(0-15)类型: 0-典型年,1-焓值极高年,2-辐射极低年,3-辐射极高年,4-气温极低年,5-气温极高年" = "Weather data source flag: bits 0-3 store source, where 0 is built-in, 1 is the DeST weather database, 2 is a public weather data set, 3 is user input, and 4-15 are other data sources; bits 4-7 store type, where 0 is typical year, 1 is extreme-high enthalpy year, 2 is extreme-low radiation year, 3 is extreme-high radiation year, 4 is extreme-low temperature year, and 5 is extreme-high temperature year.",
    "水平面总辐射(W/m^2)" = "Global horizontal radiation in W/m2.",
    "水平面散射辐射(W/m^2)" = "Diffuse horizontal radiation in W/m2.",
    "海拔" = "Elevation.",
    "消光系数" = "Extinction coefficient.",
    "灯光作息模式" = "Lighting schedule.",
    "灯光热扰分配模式" = "Lighting heat-gain distribution mode.",
    "点的ID" = "Point ID.",
    "点顺序号(第一点和第二点是这个几何形状geometry的底线(图中平面图中所示线)的起点和终点)" = "Point sequence number; the first and second points are the start and end of the geometry baseline shown in the plan drawing.",
    "热/电比率" = "Heat-to-electricity ratio.",
    "热惰性指标" = "Thermal inertia index.",
    "蒸气渗透系数(g/m.h.mmHg)" = "Vapor permeability in g/(m.h.mmHg).",
    "蒸汽渗透系数(g/m.h.mmHg)" = "Vapor permeability in g/(m.h.mmHg).",
    "用户自定义的逐时的送风含湿量，对应SCHEDULE_YEAR表中一个SCHEDULE" = "User-defined hourly supply-air humidity ratio, corresponding to a SCHEDULE_YEAR schedule.",
    "用户自定义的逐时的送风温度，对应SCHEDULE_YEAR表中一个SCHEDULE" = "User-defined hourly supply-air temperature, corresponding to a SCHEDULE_YEAR schedule.",
    "用户自定义的附加HVAC热扰，对应USER_DEF_DLL表中的用户自定义DLL的DLL_ID" = "User-defined additional HVAC heat gain, corresponding to a user-defined DLL_ID in USER_DEF_DLL.",
    "用户自定义门的计算DLL文件名，非全路径，在安装目录的USER目录下。" = "Calculation DLL file name for a user-defined door, not a full path; located under the USER directory in the installation directory.",
    "百叶片数" = "Number of louver blades.",
    "盘管最大制冷量(W/m^2)" = "Maximum coil cooling capacity in W/m2.",
    "盘管最大加热量(W/m^2)" = "Maximum coil heating capacity in W/m2.",
    "盘管并排排数" = "Number of parallel coil rows.",
    "相对方位角（单位：°）相对于ENVIRONMENT表中的南向角度的顺时针角 [ 0, 360 ) 999表示地面或者是天花板" = "Relative azimuth in degrees: clockwise angle relative to the south-direction angle in ENVIRONMENT, in [0, 360); 999 indicates ground or ceiling.",
    "相邻房间的ID(可以为室外）" = "Adjacent room ID; can be outside.",
    "省份" = "Province.",
    "省份(中文名称)" = "Province, Chinese name.",
    "省份(英文名称)" = "Province, English name.",
    "种类 1：外墙 2： 内墙 3：屋顶 4： 楼地 5：地板/天花板（如果既是地板又是天花板，算作地板），6：挑空楼板" = "Construction kind: 1 exterior wall; 2 interior wall; 3 roof; 4 ground/intermediate floor; 5 floor/ceiling, counted as floor if both; 6 exposed floor.",
    "空气层厚度(若是空气,则此数据无用,只是一个冗余数据)" = "Air layer thickness; redundant and unused when the material is air.",
    "空调开停时间的SCHEDULE_ID" = "Air-conditioning on/off schedule ID.",
    "空调开启温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID（用于独立空调系统）" = "Air-conditioning activation temperature schedule ID, corresponding to SCHEDULE_YEAR.SCHEDULE_ID, used for unitary air-conditioning systems.",
    "空调开启温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID（用于独立空调系统），耐受温度上限" = "Air-conditioning activation temperature schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID, used for unitary air-conditioning systems; upper tolerance temperature.",
    "空调开启温度ID， 对应SCHEDULE_YEAR中的SCHEDULE_ID（用于独立空调系统），耐受温度下限" = "Air-conditioning activation temperature schedule ID corresponding to SCHEDULE_YEAR.SCHEDULE_ID, used for unitary air-conditioning systems; lower tolerance temperature.",
    "空间类型标记，0普通房间，1中庭，2阳台，3天井，4楼梯间……" = "Space type flag: 0 ordinary room; 1 atrium; 2 balcony; 3 light well; 4 stairwell; and so on.",
    "窗帘反射率作息模式编号" = "Curtain reflectance schedule identifier.",
    "窗帘编号 对应系统库中SYS_CURTAIN表中的CURTAIN_ID，为0则使用缺省值。" = "Curtain identifier corresponding to SYS_CURTAIN.CURTAIN_ID in the system library; 0 uses the default value.",
    "窗户透光材料层数" = "Number of transparent material layers in the window.",
    "窗类型名" = "Window type name.",
    "窗结构编号 对应系统库中SYS_WINDOW表中的WINDOW_ID，为0则使用缺省值。" = "Window construction identifier corresponding to SYS_WINDOW.WINDOW_ID in the system library; 0 uses the default value.",
    "窗遮阳编号 对应SHADING中的ID，为0则表示没有外遮阳。(2.6版结构为避免和旧的遮阳定义模式冲突而增加)" = "Window shading identifier corresponding to SHADING.ID; 0 means no exterior shading. Added in version 2.6 to avoid conflicts with the legacy shading definition mode.",
    "第一平面，面向图上有向线段方向，左手边的平面" = "First plane: the plane on the left when facing the directed line segment in the drawing.",
    "第一平面，面向图上有向线段方向，左手边的平面(所指的surface)" = "First plane: the referenced surface on the left when facing the directed line segment in the drawing.",
    "第一平面，面向图上有向线段方向，左手边的平面（若是地板，则表示上平面）" = "First plane: the plane on the left when facing the directed line segment in the drawing; for floors, this is the upper plane.",
    "第一片遮阳板与窗口顶部或左框(垂直百叶)距离(m)" = "Distance in m from the first shading blade to the window top, or to the left frame for vertical louvers.",
    "第二平面，面向图上有向线段方向，右手边的平面" = "Second plane: the plane on the right when facing the directed line segment in the drawing.",
    "第二平面，面向图上有向线段方向，右手边的平面（若是地板，则表示下平面）" = "Second plane: the plane on the right when facing the directed line segment in the drawing; for floors, this is the lower plane.",
    "类型" = "Type.",
    "类型（待定)" = "Type, to be determined.",
    "类型(用于main_enclosure表时,用于区分开五种类型:1：外墙 2： 内墙 3：屋顶 4： 楼地 5：地板/天花板（如果既是地板又是天花板，算作地板）,用于surface表时,区分四种不同表面的对流换热系数:0-垂直外表面 1-垂直内表面 2-水平向上表面 3-水平向下表面)" = "Type code: in MAIN_ENCLOSURE, distinguishes construction types 1 exterior wall, 2 interior wall, 3 roof, 4 ground/intermediate floor, and 5 floor/ceiling counted as floor if both; in SURFACE, distinguishes convective heat-transfer coefficient types 0 vertical exterior surface, 1 vertical interior surface, 2 upward-facing horizontal surface, and 3 downward-facing horizontal surface.",
    "系统库中DOOR表中的DOOR_ID关联，为0则使用缺省值。" = "DOOR_ID reference in the system-library DOOR table; 0 uses the default value.",
    "纬度" = "Latitude.",
    "纬度: 北纬0-90, 南纬0-(-90)" = "Latitude: north latitude 0 to 90, south latitude 0 to -90.",
    "经度" = "Longitude.",
    "经度: 东经0-180, 西经0-(-180)" = "Longitude: east longitude 0 to 180, west longitude 0 to -180.",
    "结果浏览界面的选项，是否显示此曲线" = "Result-browser option indicating whether to display this curve.",
    "编号" = "Identifier.",
    "缝隙长度" = "Gap length.",
    "自定义大地的逐时地温℃" = "User-defined hourly ground temperature in degrees C.",
    "英文名" = "English name.",
    "蓄热系数(W/m^2.K)" = "Heat storage coefficient in W/m2.K.",
    "表名" = "Table name.",
    "表面吸收系数" = "Surface absorptance coefficient.",
    "表面状态编号, 预留" = "Surface state code, reserved.",
    "表面节点在对面表面（组成同一围护）所属的房间的传热方程组A矩阵中的下标" = "Index of this surface node in the heat-transfer equation A matrix of the room owning the opposite surface in the same enclosure.",
    "表面节点在表面所属的房间的传热方程组A矩阵中的下标" = "Index of this surface node in the heat-transfer equation A matrix of the room owning the surface.",
    "表面黑度" = "Surface blackness.",
    "要求的风口出口余压(Pa)" = "Required residual pressure at the air outlet in Pa.",
    "计算landa,Fai后，是否将相近/似的landarZip以提高计算速度" = "Whether to zip similar landa values after calculating landa and Fai to improve calculation speed.",
    "计算landa,fai时，是否进行Stiffen刚性化处理，刚性化处理可以增强计算的稳定性。" = "Whether to apply Stiffen rigidification when calculating landa and fai; rigidification can improve numerical stability.",
    "计算时这个围护被分成的份数" = "Number of subdivisions used for this enclosure during calculation.",
    "设备作息模式" = "Equipment schedule.",
    "设备热扰分配模式" = "Equipment heat-gain distribution mode.",
    "该建筑是否可见" = "Whether this building is visible.",
    "说明" = "Description.",
    "透光材料表中透光材料的ID" = "Transparent material ID in the transparent material table.",
    "透光标记" = "Transparency flag.",
    "透光比率" = "Transparent ratio.",
    "通风定义的含义：0:换气次数；1:附加换气次数范围" = "Ventilation definition code: 0 air changes; 1 additional air-change range.",
    "遮阳板一端至窗左边或上边(垂直百叶)距离(m)" = "Distance in m from one end of the shading panel to the left side of the window, or to the top for vertical louvers.",
    "遮阳板倾斜角度(Deg)：0表示垂直，下、右角度>0" = "Shading panel tilt angle in degrees: 0 means vertical; downward or rightward angles are positive.",
    "遮阳板另一端至窗右边或下边(垂直百叶)距离(m)" = "Distance in m from the other end of the shading panel to the right side of the window, or to the bottom for vertical louvers.",
    "遮阳板挑出宽度(m)" = "Shading panel projection width in m.",
    "遮阳板材料的反射率" = "Shading panel material reflectance.",
    "遮阳板材料的透过率" = "Shading panel material transmittance.",
    "遮阳板材料编号，指向SYS_APP_MATERIAL表，可定义厚度、透明度等信息" = "Shading panel material identifier pointing to SYS_APP_MATERIAL, where thickness, transparency, and related properties can be defined.",
    "遮阳板间距(m)" = "Shading panel spacing in m.",
    "遮阳类型，……" = "Shading type.",
    "遮阳结构编号(0表示不是结构化的遮阳定义)" = "Structured shading identifier; 0 means this is not a structured shading definition.",
    "遮阳结构造型分类：0-普通遮阳，1-水平百叶，2-垂直百叶" = "Structured shading shape category: 0 ordinary shading; 1 horizontal louver; 2 vertical louver.",
    "附注" = "Annotation.",
    "非0时表示这是个空调房间" = "Nonzero means this is an air-conditioned room.",
    "非百叶遮阳：右侧板宽度(m)" = "Non-louver shading: right side panel width in m.",
    "非百叶遮阳：右侧板高度(m)" = "Non-louver shading: right side panel height in m.",
    "非百叶遮阳：左侧板宽度(m)" = "Non-louver shading: left side panel width in m.",
    "非百叶遮阳：左侧板角度(Deg)：0表示垂直，下、右角度>0" = "Non-louver shading: left side panel angle in degrees; 0 means vertical, and downward or rightward angles are positive.",
    "非百叶遮阳：左侧板高度(m)" = "Non-louver shading: left side panel height in m.",
    "非百叶遮阳：挑帘高度(m)" = "Non-louver shading: overhang height in m.",
    "面积（单位：m2）" = "Area in m2.",
    "颜色标记" = "Color flag.",
    "风向:0-C,1-N,2-NNE,3-NE,4-ENE,5-E...9-S...13-W...16-NNW" = "Wind direction code: 0 C, 1 N, 2 NNE, 3 NE, 4 ENE, 5 E, ..., 9 S, ..., 13 W, ..., 16 NNW.",
    "风速(m/s)" = "Wind speed in m/s."
)

field_key <- paste(fields$table, fields$field, sep = "\r")
field_cn_key <- paste(fields_cn$table, fields_cn$field, sep = "\r")
description_cn <- fields_cn$description_cn[match(field_key, field_cn_key)]

if (anyNA(description_cn) || any(!nzchar(description_cn))) {
    stop("Every catalog field must have a non-empty Access field description.", call. = FALSE)
}

needs_translation <- sort(unique(description_cn[grepl("[\u4e00-\u9fff]", description_cn)]))
missing <- setdiff(needs_translation, names(translation))
if (length(missing)) {
    stop(
        paste(
            c("Missing English translations for Access descriptions:", paste0("- ", missing)),
            collapse = "\n"
        ),
        call. = FALSE
    )
}

translate_description <- function(x) {
    if (!grepl("[\u4e00-\u9fff]", x)) {
        return(x)
    }

    unname(translation[[x]])
}

fields$semantics <- vapply(description_cn, translate_description, character(1L))
fields$notes <- sub("^Access Description from CoA_Chongqin_2015; ?", "", fields$notes)
fields$notes[fields$notes == "Access Description from CoA_Chongqin_2015."] <- ""

write_schema_tsv(fields, source_fields)

cat("Updated ", nrow(fields), " catalog field semantics from Access descriptions.\n", sep = "")
