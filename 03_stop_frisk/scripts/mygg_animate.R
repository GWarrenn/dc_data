mygg_animate <- function (p = last_plot(), filename = NULL, saver = NULL, title_frame = TRUE, 
    ...) 
{
    if (is.null(p)) {
        stop("no plot to save")
    }
    built <- ggplot_build(p)
    frames <- plyr::compact(lapply(built$data, function(d) d$frame))
    titles <- plyr::compact(lapply(built$data, function(d) d$ttl))

    if (length(frames) == 0) {
        stop("No frame aesthetic found; cannot create animation")
    }
#    if (is.factor(frames[[1]])) {
        frames.unique <- sort(unique(unlist(frames)))
 #   }
#    else {
#        frames.unique <- sort(unique(do.call(c, frames)))
#    }
    frames.unique <- sort(unique(frames.unique))
    plots <- lapply(1:length(frames.unique), function(k) {
        f <- frames.unique[k]
        t <- titles[[1]][which(frames[[1]]==f)[1]]
        b <- built
        for (i in seq_along(b$data)) {
            frame_vec <- b$data[[i]]$frame
            if (!is.null(frame_vec)) {
                sub <- (frame_vec == f | is.na(frame_vec))
                if (!is.null(b$data[[i]]$cumulative)) {
                  sub <- sub | (b$data[[i]]$cumulative & (frame_vec <= 
                    f))
                }
                b$data[[i]] <- b$data[[i]][sub, ]
            }
        }
        if (title_frame) {
            if (!is.null(b$plot$labels$title)) {
                b$plot$labels$title <- paste(b$plot$labels$title, 
                  t)
            }
            else {
                b$plot$labels$title <- t
            }
        }
        b
    })
    ret <- list(plots = plots, frames = frames.unique)
    class(ret) <- "gg_animate"
    if (!is.null(filename)) {
        gganimate:::gganimate_save(ret, filename, saver, ...)
        ret$saved <- TRUE
    }
    else {
        ret$ani_opts <- list(...)
        ret$saved <- FALSE
    }
    ret
}