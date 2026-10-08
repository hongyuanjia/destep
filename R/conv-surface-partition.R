# Partition a simple polygon by safe diagonals before considering triangulation.
# Resolve concave/straight vertices first; retain every boundary segment. NULL
# requests the established fallback for openings, crowded or unsupported input.
surface__partition_by_diagonals <- function(
    surface,
    avoid_points = data.table::data.table(),
    profile = eplus_geom__profile(),
    strategy = "shortest"
) {
    count <- nrow(surface)
    if (count < 3L || count > 128L || nrow(avoid_points) > 0L) {
        return(NULL)
    }
    coordinates <- geom__coordinates(surface)
    if (anyNA(coordinates) || any(!is.finite(coordinates))) {
        return(NULL)
    }
    frame <- geom__polygon_frame(surface, profile$normal_magnitude)
    if (!frame$valid || frame$planarity_error > profile$planarity_distance) {
        return(NULL)
    }
    xy <- frame$xy
    if (anyNA(xy) || any(!is.finite(xy))) {
        return(NULL)
    }
    order <- seq_len(count)
    if (frame$signed_area < 0.0) {
        order <- rev(order)
    }
    angular <- sin(profile$angle)
    distance <- profile$coordinate_distance
    # Batch planar cross products classify turns and candidate endpoint angles.
    cross <- function(a, b) a[, 1L] * b[, 2L] - a[, 2L] * b[, 1L]
    # Polygon splitting is stateful; bounded stacks avoid growing row tables.
    pending <- vector("list", count)
    pending[[1L]] <- order
    queue <- 1L
    finished <- vector("list", count)
    total <- 0L
    while (queue > 0L) {
        vertices <- pending[[queue]]
        queue <- queue - 1L
        points <- xy[vertices, , drop = FALSE]
        n <- nrow(points)
        previous <- c(n, seq_len(n - 1L))
        following <- c(seq.int(2L, n), 1L)
        incoming <- points - points[previous, , drop = FALSE]
        outgoing <- points[following, , drop = FALSE] - points
        in_length <- sqrt(rowSums(incoming^2))
        out_length <- sqrt(rowSums(outgoing^2))
        if (any(in_length < distance)) {
            return(NULL)
        }
        turn <- cross(incoming, outgoing)
        threshold <- angular * in_length * out_length
        bad <- turn <= threshold
        # A reflex corner costs two unresolved turns, a straight junction one;
        # every accepted cut must reduce this nonnegative integer potential.
        weight <- as.integer(bad) + as.integer(turn < -threshold)
        if (!any(bad)) {
            total <- total + 1L
            finished[[total]] <- vertices
            next
        }
        # Only diagonals incident to unresolved vertices can reduce the number
        # of required parts. Score all such pairs together before visibility.
        pair <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        a <- pair[, 1L]
        b <- pair[, 2L]
        keep <- b != a + 1L & !(a == 1L & b == n) & (bad[a] | bad[b])
        pair <- pair[keep, , drop = FALSE]
        if (!nrow(pair)) {
            return(NULL)
        }
        a <- pair[, 1L]
        b <- pair[, 2L]
        direction <- points[b, , drop = FALSE] - points[a, , drop = FALSE]
        length <- sqrt(rowSums(direction^2))
        # An axis-aligned cut may replace a reflex corner by a straight one.
        # Permit that intermediate state only when total unresolved weight
        # decreases; final polygons still require every corner to be strict.
        first <- list(
            incoming[a, , drop = FALSE],
            -direction,
            incoming[b, , drop = FALSE],
            direction
        )
        second <- list(
            direction,
            outgoing[a, , drop = FALSE],
            -direction,
            outgoing[b, , drop = FALSE]
        )
        keep <- length >= distance
        benefit <- weight[a] + weight[b]
        for (endpoint in seq_len(4L)) {
            lhs <- first[[endpoint]]
            rhs <- second[[endpoint]]
            value <- cross(lhs, rhs)
            limit <- angular * sqrt(rowSums(lhs^2) * rowSums(rhs^2))
            straight <- abs(value) <= limit
            keep <- keep &
                value >= -limit &
                (!straight | rowSums(lhs * rhs) > 0)
            benefit <- benefit - as.integer(straight)
        }
        keep <- keep & benefit > 0L
        pair <- pair[keep, , drop = FALSE]
        length <- length[keep]
        benefit <- benefit[keep]
        if (!nrow(pair)) {
            return(NULL)
        }
        a <- pair[, 1L]
        b <- pair[, 2L]
        unresolved <- c(0L, cumsum(bad))
        inside <- unresolved[b] - unresolved[a + 1L]
        outside <- sum(bad) - inside - bad[a] - bad[b]
        secondary <- switch(
            strategy,
            shortest = length,
            longest = -length,
            balanced = pmax(inside, outside),
            stop("Unknown diagonal ranking strategy.")
        )
        ranked <- order(
            -benefit,
            secondary,
            length,
            a,
            b
        )
        selected <- NULL
        # Visibility tests are vectorized over edges; stop at the first safe
        # ranked cut rather than constructing all candidate polygon copies.
        for (candidate in ranked) {
            i <- a[[candidate]]
            j <- b[[candidate]]
            delta <- points[j, ] - points[i, ]
            relative <- sweep(points, 2L, points[i, ], "-")
            along <- as.vector(relative %*% delta / sum(delta^2))
            perpendicular <- abs(
                relative[, 1L] * delta[[2L]] - relative[, 2L] * delta[[1L]]
            ) /
                length[[candidate]]
            other <- setdiff(seq_len(n), c(i, j))
            if (
                any(
                    along[other] > 0 &
                        along[other] < 1 &
                        perpendicular[other] < distance
                )
            ) {
                next
            }
            # A proper intersection with any nonincident boundary edge makes
            # this chord exterior or crossing, even when endpoint angles fit.
            edges <- which(!seq_len(n) %in% c(i, j) & !following %in% c(i, j))
            segment <- outgoing[edges, , drop = FALSE]
            offset <- relative[edges, , drop = FALSE]
            denominator <- delta[[1L]] *
                segment[, 2L] -
                delta[[2L]] * segment[, 1L]
            active <- abs(denominator) > profile$intersection
            t <- cross(
                offset[active, , drop = FALSE],
                segment[active, , drop = FALSE]
            ) /
                denominator[active]
            u <- (offset[active, 1L] *
                delta[[2L]] -
                offset[active, 2L] * delta[[1L]]) /
                denominator[active]
            if (any(t >= 0 & t <= 1 & u >= 0 & u <= 1)) {
                next
            }
            selected <- c(i, j)
            break
        }
        if (is.null(selected)) {
            return(NULL)
        }
        i <- selected[[1L]]
        j <- selected[[2L]]
        pending[[queue + 1L]] <- vertices[i:j]
        pending[[queue + 2L]] <- vertices[c(j:n, seq_len(i))]
        queue <- queue + 2L
    }
    data.table::rbindlist(lapply(seq_len(total), function(part) {
        value <- data.table::copy(surface[finished[[part]]])
        data.table::set(value, j = "PART", value = part)
        data.table::set(
            value,
            j = "POINT_NO",
            value = seq_len(nrow(value)) - 1L
        )
        value
    }))
}

# Preserve convex regions with safe diagonals; triangulation remains the bounded
# fallback for openings and polygons the direct strategy cannot fully resolve.
surface__partition_polygon <- function(
    surface,
    avoid_points = data.table::data.table(),
    profile = eplus_geom__profile()
) {
    direct <- surface__partition_by_diagonals(surface, avoid_points, profile)
    if (!is.null(direct)) {
        return(direct)
    }
    surface__triangulate_polygon(surface, avoid_points, profile)
}
