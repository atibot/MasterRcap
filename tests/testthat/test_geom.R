test_that("geom_timeline returns a ggplot", {
    g <- read_eq_clean_data() %>%
        dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("USA", "CANADA")) %>%
        dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                      EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
        ggplot2::ggplot(ggplot2::aes(x = DATE,
                                     y = COUNTRY,
                                     colour = TOTAL_DEATHS,
                                     size = EQ_PRIMARY
                                     )) +
        geom_timeline()

    expect_is(g, "ggplot")
})


test_that("geom_timeline_label returns also a ggplot", {
    g <- read_eq_clean_data() %>%
        dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("USA", "CANADA")) %>%
        dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                      EQ_PRIMARY = as.numeric(EQ_PRIMARY)) %>%
        ggplot2::ggplot(ggplot2::aes(x = DATE,
                                     y = COUNTRY,
                                     colour = TOTAL_DEATHS,
                                     size = EQ_PRIMARY
                                     )) +
        geom_timeline() +
        geom_timeline_label()

    expect_is(g, "ggplot")

})

test_that("theme_timeline returns a theme", {
    t <- theme_timeline()
    expect_is(t, "theme")
    expect_is(t, "gg")
})
