context("clean spelling tests")


corrections <- data.frame(
  bad = c("foubar", "foobr", "fubar", ".missing", "unknown"),
  good = c("foobar", "foobar", "foobar", "missing", "missing"),
  stringsAsFactors = FALSE
)

my_data <- c(letters[1:5], "foubar", "foobr", "fubar", NA, "", "unknown", "fumar")
cleaned_data <- c(letters[1:5], "foobar", "foobar", "foobar", "missing", "missing", "missing", "fumar")

test_that("match_vec throws an error with no data or a data frame", {

  expect_error(match_vec(), "x must be coerceable to a character")
  expect_error(match_vec(corrections), "x must be coerceable to a character")

})


test_that("match_vec throws an error with no dictionary", {

  bad_df   <- data.frame(a = 1:10)
  bad_df$b <- as.list(1:10) # list column
  expect_error(match_vec(my_data), "dictionary must be a data frame")
  expect_error(match_vec(my_data, my_data), "dictionary must be a data frame")
  expect_error(match_vec(my_data, bad_df[1]), "dictionary must be a data frame with at least two columns")
  expect_error(match_vec(my_data, bad_df), "dictionary must have two columns coerceable to a character")

})

test_that("match_vec will preserve element names", {
  md <- setNames(my_data, letters[1:length(my_data)])
  expect_identical(names(match_vec(md, corrections)), names(md))
})



test_that("match_vec can take different arrangements of dictionary", {

  expect_identical(match_vec(my_data, corrections[2:1], from = 2, to = 1),
                   cleaned_data)
  expect_identical(match_vec(my_data, corrections[2:1], from = "bad", to = "good"),
                   cleaned_data)
  expect_identical(match_vec(my_data, corrections[2:1], from = "bad", to = 1),
                   cleaned_data)
  expect_identical(match_vec(my_data, corrections[2:1], from = 2, to = "good"),
                   cleaned_data)

})

test_that("match_vec throws an error if the columns are incorrect", {

  expect_error(match_vec(my_data, corrections, from = "you", to = "me"),
               "`from` and `to` must refer to columns in the dictionary",
               fixed = TRUE)
  expect_error(match_vec(my_data, corrections, from = 1, to = "me"),
               "`from` and `to` must refer to columns in the dictionary",
               fixed = TRUE)
  expect_error(match_vec(my_data, corrections, from = "me", to = 2),
               "`from` and `to` must refer to columns in the dictionary",
               fixed = TRUE)
  expect_error(match_vec(my_data, corrections, from = 6, to = 9),
               "`from` and `to` must refer to columns in the dictionary",
               fixed = TRUE)
  expect_error(match_vec(my_data, corrections, from = 0, to = 2),
               "`from` and `to` must refer to columns in the dictionary",
               fixed = TRUE)

})

test_that("regex is possible", {

  datf <- corrections
  datf <- datf[2:5, ]
  # replace foobar, foubar, foobr, fubar, etc with foobar
  datf$bad[1]  <- ".regex f[ou][^m][a-z]+r"
  datf$good[1] <- "foobar"

  # replace the literal no importa with dang (shouldn't do anything)
  datf$bad[2]  <- ".regex no importa"
  datf$good[2] <- "dang"

  # with regex anchors
  expect_identical(match_vec(c(my_data, "dang foobr"), datf),
                   c(cleaned_data, "dang foobr"))

  # without anchors
  expect_identical(match_vec(c(my_data, "dang foobr fuuuuber"), datf, anchor_regex = FALSE),
                   c(cleaned_data, "dang foobar foobar"))


})

test_that("logical values can be coerced", {

  res <- match_vec(1:5 > 2, data.frame(a = c(FALSE, TRUE), b = c("hell", "yeah")))
  expected <- rep(c("hell", "yeah"), c(2, 3))
  expect_identical(res, expected)

})

test_that("match_vec will clean everything defined in the dictionary", {

  expect_identical(match_vec(my_data, corrections), cleaned_data)

})


test_that("match_vec will work with a matrix", {

  expect_identical(match_vec(my_data, as.matrix(corrections)), cleaned_data)

})

test_that("match_vec will throw a warning if there is a missing value", {

  corrections2 <- corrections
  missme <- corrections[[1]] == ".missing"
  corrections2[[1]][missme] <- NA
  expect_warning(match_vec(my_data, corrections2),
                 "If you want to indicate missing data, use the '.missing' keyword")

})

test_that("match_vec will throw a warning if there are duplicated keys", {

  corrections2 <- rbind(corrections, corrections[1:2, ])
  expect_warning(match_vec(my_data, corrections2),
                 "Duplicate keys were found.+.foubar., .foobr.[^,]")

})


test_that("match_vec will take in a default value", {

  with_default <- rbind(corrections, c(bad = ".default", good = "check me"))
  cleaned_default <- cleaned_data
  cleaned_default[!cleaned_default %in% with_default$good] <- "check me"
  d_warn <- "'a', 'b', 'c', 'd', 'e', 'fumar' were changed to the default value \\('check me'\\)"
  expect_warning({
    my_cleaned <- match_vec(my_data, with_default)
  }, d_warn)
  expect_identical(my_cleaned, cleaned_default)

})


test_that("nothing to default will not throw a warning", {


  with_default <- rbind(corrections, c(bad = ".default", good = "unknown"))
  cleaned_default <- cleaned_data[6:7]
  expect_failure(expect_warning(match_vec(my_data[6:7], with_default)))


})


test_that("match_vec will be silent if the data are already cleaned", {

  expect_failure(expect_warning(match_vec(cleaned_data, corrections)))

})


test_that("match_vec will throw a warning for completely unknown data", {

  expect_warning(match_vec(letters[1:4], corrections),
                 "None of the variables in letters\\[1:4\\] were found in corrections. Did you use the correct dictionary?")

})

test_that("match_vec maintains order of original data", {

  abc_dict <- data.frame(c(letters, "zz"), c(LETTERS, "Z"), stringsAsFactors = FALSE)
  lfac <- sample(c("zz", sample(letters, 19, replace = TRUE)))
  # Letters are reversed
  upps <- match_vec(factor(lfac, c("zz", rev(letters))), abc_dict)
  expect_true(all(upps %in% LETTERS))
  # The results respects the dictionary
  expect_identical(levels(upps), LETTERS)
  expect_false("zz" %in% upps)

  # untranslated factors are placed at the end
  upps2 <- match_vec(factor(lfac, c("hey", "there", "zz", rev(letters))), abc_dict)
  expect_true(all(upps2 %in% LETTERS))
  expect_identical(levels(upps2), c(LETTERS, "hey", "there"))
  expect_false("zz" %in% upps2)

})
