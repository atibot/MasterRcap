test_that("eq_map draw a map", {
    m <- read_eq_clean_data() %>%
        dplyr::filter(YEAR > 2000 & !IS_BC & COUNTRY %in% c("MEXICO")) %>%
        dplyr::mutate(popup_text = eq_create_label(.)) %>%
        eq_map(annot_col = "popup_text")
    expect_is(m, "leaflet")
    expect_is(m, "htmlwidget")
})


test_that("eq_create_label creates label", {
    data <- data.frame("Pompeii", "5.2", NA)
    colnames(data) <- c("LOCATION_NAME", "EQ_PRIMARY", "TOTAL_DEATHS")
    labels <- data %>% eq_create_label()
    expect_equal(length(labels), 1)

    label <- labels[1]
    expect_equal(label, "<b>Location:</b> Pompeii <br /> <b>Magnitude:</b> 5.2 <br />")
})
