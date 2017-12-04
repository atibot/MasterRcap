test_that("eq_map draw a map", {
    m <- eq_clean_data() %>%
        dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("MEXICO")) %>%
        dplyr::mutate(popup_text = eq_create_label(.)) %>%
        eq_map(annot_col = "popup_text")
    expect_is(m, "leaflet")
    expect_is(m, "htmlwidget")
})


