




```r
knitr::asis_output(data_info)
```




### Metadata

#### Description

```r
if (exists("name", meta)) {
  glue::glue(
    "__Dataset name__: {name}",
    .envir = meta)
}
```

__Dataset name__: tidy_data

```r
cat(description)
```

The dataset has N=180 rows and 68 columns.
180 rows have no missing values on any column.






- __Date published__: 2019-01-01

```r
meta <- meta[setdiff(names(meta),
                     c("creator", "datePublished", "identifier",
                       "url", "citation", "spatialCoverage", 
                       "temporalCoverage", "description", "name"))]
pander::pander(meta)
```



  * **keywords**: _volunteer_, _activity_, _timeBodyAccelerometer-mean()-X_, _timeBodyAccelerometer-mean()-Y_, _timeBodyAccelerometer-mean()-Z_, _timeGravityAccelerometer-mean()-X_, _timeGravityAccelerometer-mean()-Y_, _timeGravityAccelerometer-mean()-Z_, _timeBodyAccelerometerJerk-mean()-X_, _timeBodyAccelerometerJerk-mean()-Y_, _timeBodyAccelerometerJerk-mean()-Z_, _timeBodyGyroscope-mean()-X_, _timeBodyGyroscope-mean()-Y_, _timeBodyGyroscope-mean()-Z_, _timeBodyGyroscopeJerk-mean()-X_, _timeBodyGyroscopeJerk-mean()-Y_, _timeBodyGyroscopeJerk-mean()-Z_, _timeBodyAccelerometerMagnitude-mean()_, _timeGravityAccelerometerMagnitude-mean()_, _timeBodyAccelerometerJerkMagnitude-mean()_, _timeBodyGyroscopeMagnitude-mean()_, _timeBodyGyroscopeJerkMagnitude-mean()_, _frequencyBodyAccelerometer-mean()-X_, _frequencyBodyAccelerometer-mean()-Y_, _frequencyBodyAccelerometer-mean()-Z_, _frequencyBodyAccelerometerJerk-mean()-X_, _frequencyBodyAccelerometerJerk-mean()-Y_, _frequencyBodyAccelerometerJerk-mean()-Z_, _frequencyBodyGyroscope-mean()-X_, _frequencyBodyGyroscope-mean()-Y_, _frequencyBodyGyroscope-mean()-Z_, _frequencyBodyAccelerometerMagnitude-mean()_, _frequencyBodyAccelerometerJerkMagnitude-mean()_, _frequencyBodyGyroscopeMagnitude-mean()_, _frequencyBodyGyroscopeJerkMagnitude-mean()_, _timeBodyAccelerometer-std()-X_, _timeBodyAccelerometer-std()-Y_, _timeBodyAccelerometer-std()-Z_, _timeGravityAccelerometer-std()-X_, _timeGravityAccelerometer-std()-Y_, _timeGravityAccelerometer-std()-Z_, _timeBodyAccelerometerJerk-std()-X_, _timeBodyAccelerometerJerk-std()-Y_, _timeBodyAccelerometerJerk-std()-Z_, _timeBodyGyroscope-std()-X_, _timeBodyGyroscope-std()-Y_, _timeBodyGyroscope-std()-Z_, _timeBodyGyroscopeJerk-std()-X_, _timeBodyGyroscopeJerk-std()-Y_, _timeBodyGyroscopeJerk-std()-Z_, _timeBodyAccelerometerMagnitude-std()_, _timeGravityAccelerometerMagnitude-std()_, _timeBodyAccelerometerJerkMagnitude-std()_, _timeBodyGyroscopeMagnitude-std()_, _timeBodyGyroscopeJerkMagnitude-std()_, _frequencyBodyAccelerometer-std()-X_, _frequencyBodyAccelerometer-std()-Y_, _frequencyBodyAccelerometer-std()-Z_, _frequencyBodyAccelerometerJerk-std()-X_, _frequencyBodyAccelerometerJerk-std()-Y_, _frequencyBodyAccelerometerJerk-std()-Z_, _frequencyBodyGyroscope-std()-X_, _frequencyBodyGyroscope-std()-Y_, _frequencyBodyGyroscope-std()-Z_, _frequencyBodyAccelerometerMagnitude-std()_, _frequencyBodyAccelerometerJerkMagnitude-std()_, _frequencyBodyGyroscopeMagnitude-std()_ and _frequencyBodyGyroscopeJerkMagnitude-std()_

<!-- end of list -->



```r
knitr::asis_output(survey_overview)
```


## Variables


```r
knitr::asis_output(paste0(scales_items, sep = "\n\n\n", collapse = "\n\n\n"))
```




### volunteer {#volunteer .tabset}



#### Distribution {#volunteer_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/volunteer_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#volunteer_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name      |data_type |missing |complete |n   |mean |sd   |p0 |p25 |p50  |p75 |p100 |hist     |
|:---------|:---------|:-------|:--------|:---|:----|:----|:--|:---|:----|:---|:----|:--------|
|volunteer |integer   |0       |180      |180 |15.5 |8.68 |1  |8   |15.5 |23  |30   |<U+2587><U+2587><U+2586><U+2587><U+2587><U+2586><U+2587><U+2587> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### activity {#activity .tabset}



#### Distribution {#activity_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/activity_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#activity_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name     |data_type |missing |complete |n   |empty |n_unique |min |max |
|:--------|:---------|:-------|:--------|:---|:-----|:--------|:---|:---|
|activity |character |0       |180      |180 |0     |6        |6   |18  |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-mean()-X {#timeBodyAccelerometer_mean___X .tabset}



#### Distribution {#timeBodyAccelerometer_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean |sd    |p0   |p25  |p50  |p75  |p100 |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:----|:-----|:----|:----|:----|:----|:----|:--------|
|timeBodyAccelerometer-mean()-X |numeric   |0       |180      |180 |0.27 |0.012 |0.22 |0.27 |0.28 |0.28 |0.3  |<U+2581><U+2581><U+2581><U+2581><U+2582><U+2587><U+2582><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-mean()-Y {#timeBodyAccelerometer_mean___Y .tabset}



#### Distribution {#timeBodyAccelerometer_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean   |sd     |p0     |p25   |p50    |p75    |p100    |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:------|:------|:------|:-----|:------|:------|:-------|:--------|
|timeBodyAccelerometer-mean()-Y |numeric   |0       |180      |180 |-0.018 |0.0058 |-0.041 |-0.02 |-0.017 |-0.015 |-0.0013 |<U+2581><U+2581><U+2582><U+2582><U+2587><U+2583><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-mean()-Z {#timeBodyAccelerometer_mean___Z .tabset}



#### Distribution {#timeBodyAccelerometer_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean  |sd     |p0    |p25   |p50   |p75  |p100   |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:-----|:------|:-----|:-----|:-----|:----|:------|:--------|
|timeBodyAccelerometer-mean()-Z |numeric   |0       |180      |180 |-0.11 |0.0096 |-0.15 |-0.11 |-0.11 |-0.1 |-0.075 |<U+2581><U+2581><U+2581><U+2582><U+2587><U+2583><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-mean()-X {#timeGravityAccelerometer_mean___X .tabset}



#### Distribution {#timeGravityAccelerometer_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean |sd   |p0    |p25  |p50  |p75  |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:----|:----|:-----|:----|:----|:----|:----|:--------|
|timeGravityAccelerometer-mean()-X |numeric   |0       |180      |180 |0.7  |0.49 |-0.68 |0.84 |0.92 |0.94 |0.97 |<U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-mean()-Y {#timeGravityAccelerometer_mean___Y .tabset}



#### Distribution {#timeGravityAccelerometer_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean   |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:------|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeGravityAccelerometer-mean()-Y |numeric   |0       |180      |180 |-0.016 |0.35 |-0.48 |-0.23 |-0.13 |0.088 |0.96 |<U+2582><U+2587><U+2585><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-mean()-Z {#timeGravityAccelerometer_mean___Z .tabset}



#### Distribution {#timeGravityAccelerometer_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean  |sd   |p0   |p25   |p50   |p75  |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:-----|:----|:----|:-----|:-----|:----|:----|:--------|
|timeGravityAccelerometer-mean()-Z |numeric   |0       |180      |180 |0.074 |0.29 |-0.5 |-0.12 |0.024 |0.15 |0.96 |<U+2581><U+2585><U+2587><U+2586><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-mean()-X {#timeBodyAccelerometerJerk_mean___X .tabset}



#### Distribution {#timeBodyAccelerometerJerk_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean  |sd    |p0    |p25   |p50   |p75   |p100 |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:-----|:-----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometerJerk-mean()-X |numeric   |0       |180      |180 |0.079 |0.013 |0.043 |0.074 |0.076 |0.083 |0.13 |<U+2581><U+2581><U+2587><U+2587><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-mean()-Y {#timeBodyAccelerometerJerk_mean___Y .tabset}



#### Distribution {#timeBodyAccelerometerJerk_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean   |sd    |p0     |p25     |p50    |p75   |p100  |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:------|:-----|:------|:-------|:------|:-----|:-----|:--------|
|timeBodyAccelerometerJerk-mean()-Y |numeric   |0       |180      |180 |0.0076 |0.014 |-0.039 |0.00047 |0.0095 |0.013 |0.057 |<U+2581><U+2581><U+2582><U+2586><U+2587><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-mean()-Z {#timeBodyAccelerometerJerk_mean___Z .tabset}



#### Distribution {#timeBodyAccelerometerJerk_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean   |sd    |p0     |p25    |p50     |p75   |p100  |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:------|:-----|:------|:------|:-------|:-----|:-----|:--------|
|timeBodyAccelerometerJerk-mean()-Z |numeric   |0       |180      |180 |-0.005 |0.013 |-0.067 |-0.011 |-0.0039 |0.002 |0.038 |<U+2581><U+2581><U+2581><U+2581><U+2587><U+2586><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-mean()-X {#timeBodyGyroscope_mean___X .tabset}



#### Distribution {#timeBodyGyroscope_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                       |data_type |missing |complete |n   |mean   |sd    |p0    |p25    |p50    |p75    |p100 |hist     |
|:--------------------------|:---------|:-------|:--------|:---|:------|:-----|:-----|:------|:------|:------|:----|:--------|
|timeBodyGyroscope-mean()-X |numeric   |0       |180      |180 |-0.032 |0.054 |-0.21 |-0.047 |-0.029 |-0.017 |0.19 |<U+2581><U+2581><U+2582><U+2587><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-mean()-Y {#timeBodyGyroscope_mean___Y .tabset}



#### Distribution {#timeBodyGyroscope_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                       |data_type |missing |complete |n   |mean   |sd    |p0   |p25   |p50    |p75    |p100  |hist     |
|:--------------------------|:---------|:-------|:--------|:---|:------|:-----|:----|:-----|:------|:------|:-----|:--------|
|timeBodyGyroscope-mean()-Y |numeric   |0       |180      |180 |-0.074 |0.036 |-0.2 |-0.09 |-0.073 |-0.061 |0.027 |<U+2581><U+2581><U+2581><U+2583><U+2587><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-mean()-Z {#timeBodyGyroscope_mean___Z .tabset}



#### Distribution {#timeBodyGyroscope_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                       |data_type |missing |complete |n   |mean  |sd    |p0     |p25   |p50   |p75 |p100 |hist     |
|:--------------------------|:---------|:-------|:--------|:---|:-----|:-----|:------|:-----|:-----|:---|:----|:--------|
|timeBodyGyroscope-mean()-Z |numeric   |0       |180      |180 |0.087 |0.036 |-0.072 |0.075 |0.085 |0.1 |0.18 |<U+2581><U+2581><U+2581><U+2581><U+2587><U+2587><U+2582><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-mean()-X {#timeBodyGyroscopeJerk_mean___X .tabset}



#### Distribution {#timeBodyGyroscopeJerk_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean   |sd    |p0    |p25  |p50    |p75    |p100   |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:------|:-----|:-----|:----|:------|:------|:------|:--------|
|timeBodyGyroscopeJerk-mean()-X |numeric   |0       |180      |180 |-0.096 |0.023 |-0.16 |-0.1 |-0.099 |-0.091 |-0.022 |<U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-mean()-Y {#timeBodyGyroscopeJerk_mean___Y .tabset}



#### Distribution {#timeBodyGyroscopeJerk_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean   |sd     |p0     |p25    |p50    |p75    |p100   |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:------|:------|:------|:------|:------|:------|:------|:--------|
|timeBodyGyroscopeJerk-mean()-Y |numeric   |0       |180      |180 |-0.043 |0.0095 |-0.077 |-0.046 |-0.041 |-0.038 |-0.013 |<U+2581><U+2581><U+2581><U+2582><U+2587><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-mean()-Z {#timeBodyGyroscopeJerk_mean___Z .tabset}



#### Distribution {#timeBodyGyroscopeJerk_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean   |sd    |p0     |p25    |p50    |p75    |p100    |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:------|:-----|:------|:------|:------|:------|:-------|:--------|
|timeBodyGyroscopeJerk-mean()-Z |numeric   |0       |180      |180 |-0.055 |0.012 |-0.092 |-0.062 |-0.053 |-0.049 |-0.0069 |<U+2581><U+2581><U+2583><U+2587><U+2583><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerMagnitude-mean() {#timeBodyAccelerometerMagnitude_mean__ .tabset}



#### Distribution {#timeBodyAccelerometerMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                  |data_type |missing |complete |n   |mean |sd   |p0    |p25   |p50   |p75    |p100 |hist     |
|:-------------------------------------|:---------|:-------|:--------|:---|:----|:----|:-----|:-----|:-----|:------|:----|:--------|
|timeBodyAccelerometerMagnitude-mean() |numeric   |0       |180      |180 |-0.5 |0.47 |-0.99 |-0.96 |-0.48 |-0.092 |0.64 |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometerMagnitude-mean() {#timeGravityAccelerometerMagnitude_mean__ .tabset}



#### Distribution {#timeGravityAccelerometerMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometerMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometerMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                     |data_type |missing |complete |n   |mean |sd   |p0    |p25   |p50   |p75    |p100 |hist     |
|:----------------------------------------|:---------|:-------|:--------|:---|:----|:----|:-----|:-----|:-----|:------|:----|:--------|
|timeGravityAccelerometerMagnitude-mean() |numeric   |0       |180      |180 |-0.5 |0.47 |-0.99 |-0.96 |-0.48 |-0.092 |0.64 |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerkMagnitude-mean() {#timeBodyAccelerometerJerkMagnitude_mean__ .tabset}



#### Distribution {#timeBodyAccelerometerJerkMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerkMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerkMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                      |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometerJerkMagnitude-mean() |numeric   |0       |180      |180 |-0.61 |0.4 |-0.99 |-0.98 |-0.82 |-0.25 |0.43 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeMagnitude-mean() {#timeBodyGyroscopeMagnitude_mean__ .tabset}



#### Distribution {#timeBodyGyroscopeMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeMagnitude-mean() |numeric   |0       |180      |180 |-0.57 |0.4 |-0.98 |-0.95 |-0.66 |-0.22 |0.42 |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerkMagnitude-mean() {#timeBodyGyroscopeJerkMagnitude_mean__ .tabset}



#### Distribution {#timeBodyGyroscopeJerkMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerkMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerkMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                  |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100  |hist     |
|:-------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:-----|:--------|
|timeBodyGyroscopeJerkMagnitude-mean() |numeric   |0       |180      |180 |-0.74 |0.28 |-1 |-0.99 |-0.86 |-0.51 |0.088 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-mean()-X {#frequencyBodyAccelerometer_mean___X .tabset}



#### Distribution {#frequencyBodyAccelerometer_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometer-mean()-X |numeric   |0       |180      |180 |-0.58 |0.43 |-1 |-0.98 |-0.77 |-0.22 |0.54 |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-mean()-Y {#frequencyBodyAccelerometer_mean___Y .tabset}



#### Distribution {#frequencyBodyAccelerometer_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75    |p100 |hist     |
|:-----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:------|:----|:--------|
|frequencyBodyAccelerometer-mean()-Y |numeric   |0       |180      |180 |-0.49 |0.48 |-0.99 |-0.95 |-0.59 |-0.063 |0.52 |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-mean()-Z {#frequencyBodyAccelerometer_mean___Z .tabset}



#### Distribution {#frequencyBodyAccelerometer_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometer-mean()-Z |numeric   |0       |180      |180 |-0.63 |0.36 |-0.99 |-0.96 |-0.72 |-0.32 |0.28 |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-mean()-X {#frequencyBodyAccelerometerJerk_mean___X .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                    |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerk-mean()-X |numeric   |0       |180      |180 |-0.61 |0.4 |-0.99 |-0.98 |-0.81 |-0.28 |0.47 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-mean()-Y {#frequencyBodyAccelerometerJerk_mean___Y .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                    |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75  |p100 |hist     |
|:---------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:----|:----|:--------|
|frequencyBodyAccelerometerJerk-mean()-Y |numeric   |0       |180      |180 |-0.59 |0.41 |-0.99 |-0.97 |-0.78 |-0.2 |0.28 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-mean()-Z {#frequencyBodyAccelerometerJerk_mean___Z .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                    |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerk-mean()-Z |numeric   |0       |180      |180 |-0.71 |0.3 |-0.99 |-0.98 |-0.87 |-0.47 |0.16 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-mean()-X {#frequencyBodyGyroscope_mean___X .tabset}



#### Distribution {#frequencyBodyGyroscope_mean___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_mean___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_mean___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                            |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscope-mean()-X |numeric   |0       |180      |180 |-0.64 |0.35 |-0.99 |-0.97 |-0.73 |-0.34 |0.47 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-mean()-Y {#frequencyBodyGyroscope_mean___Y .tabset}



#### Distribution {#frequencyBodyGyroscope_mean___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_mean___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_mean___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                            |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscope-mean()-Y |numeric   |0       |180      |180 |-0.68 |0.33 |-0.99 |-0.97 |-0.81 |-0.45 |0.33 |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-mean()-Z {#frequencyBodyGyroscope_mean___Z .tabset}



#### Distribution {#frequencyBodyGyroscope_mean___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_mean___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_mean___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                            |data_type |missing |complete |n   |mean |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-------------------------------|:---------|:-------|:--------|:---|:----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscope-mean()-Z |numeric   |0       |180      |180 |-0.6 |0.38 |-0.99 |-0.96 |-0.79 |-0.26 |0.49 |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerMagnitude-mean() {#frequencyBodyAccelerometerMagnitude_mean__ .tabset}



#### Distribution {#frequencyBodyAccelerometerMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                       |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:------------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerMagnitude-mean() |numeric   |0       |180      |180 |-0.54 |0.45 |-0.99 |-0.96 |-0.67 |-0.16 |0.59 |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerkMagnitude-mean() {#frequencyBodyAccelerometerJerkMagnitude_mean__ .tabset}



#### Distribution {#frequencyBodyAccelerometerJerkMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerkMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerkMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                           |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:----------------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerkMagnitude-mean() |numeric   |0       |180      |180 |-0.58 |0.43 |-0.99 |-0.98 |-0.79 |-0.19 |0.54 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscopeMagnitude-mean() {#frequencyBodyGyroscopeMagnitude_mean__ .tabset}



#### Distribution {#frequencyBodyGyroscopeMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscopeMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscopeMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                   |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:--------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscopeMagnitude-mean() |numeric   |0       |180      |180 |-0.67 |0.32 |-0.99 |-0.96 |-0.77 |-0.41 |0.2  |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscopeJerkMagnitude-mean() {#frequencyBodyGyroscopeJerkMagnitude_mean__ .tabset}



#### Distribution {#frequencyBodyGyroscopeJerkMagnitude_mean___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscopeJerkMagnitude_mean___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscopeJerkMagnitude_mean___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                       |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100 |hist     |
|:------------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscopeJerkMagnitude-mean() |numeric   |0       |180      |180 |-0.76 |0.26 |-1 |-0.98 |-0.88 |-0.58 |0.15 |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-std()-X {#timeBodyAccelerometer_std___X .tabset}



#### Distribution {#timeBodyAccelerometer_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75  |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:----|:----|:--------|
|timeBodyAccelerometer-std()-X |numeric   |0       |180      |180 |-0.56 |0.45 |-1 |-0.98 |-0.75 |-0.2 |0.63 |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-std()-Y {#timeBodyAccelerometer_std___Y .tabset}



#### Distribution {#timeBodyAccelerometer_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75    |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:------|:----|:--------|
|timeBodyAccelerometer-std()-Y |numeric   |0       |180      |180 |-0.46 |0.5 |-0.99 |-0.94 |-0.51 |-0.031 |0.62 |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometer-std()-Z {#timeBodyAccelerometer_std___Z .tabset}



#### Distribution {#timeBodyAccelerometer_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometer_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometer_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean  |sd  |p0    |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:-----|:---|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometer-std()-Z |numeric   |0       |180      |180 |-0.58 |0.4 |-0.99 |-0.95 |-0.65 |-0.23 |0.61 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-std()-X {#timeGravityAccelerometer_std___X .tabset}



#### Distribution {#timeGravityAccelerometer_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                             |data_type |missing |complete |n   |mean  |sd    |p0 |p25   |p50   |p75   |p100  |hist     |
|:--------------------------------|:---------|:-------|:--------|:---|:-----|:-----|:--|:-----|:-----|:-----|:-----|:--------|
|timeGravityAccelerometer-std()-X |numeric   |0       |180      |180 |-0.96 |0.025 |-1 |-0.98 |-0.97 |-0.95 |-0.83 |<U+2587><U+2586><U+2585><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-std()-Y {#timeGravityAccelerometer_std___Y .tabset}



#### Distribution {#timeGravityAccelerometer_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                             |data_type |missing |complete |n   |mean  |sd    |p0    |p25   |p50   |p75   |p100  |hist     |
|:--------------------------------|:---------|:-------|:--------|:---|:-----|:-----|:-----|:-----|:-----|:-----|:-----|:--------|
|timeGravityAccelerometer-std()-Y |numeric   |0       |180      |180 |-0.95 |0.033 |-0.99 |-0.97 |-0.96 |-0.94 |-0.64 |<U+2587><U+2585><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometer-std()-Z {#timeGravityAccelerometer_std___Z .tabset}



#### Distribution {#timeGravityAccelerometer_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometer_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometer_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                             |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100  |hist     |
|:--------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:-----|:--------|
|timeGravityAccelerometer-std()-Z |numeric   |0       |180      |180 |-0.94 |0.04 |-0.99 |-0.96 |-0.95 |-0.92 |-0.61 |<U+2587><U+2586><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-std()-X {#timeBodyAccelerometerJerk_std___X .tabset}



#### Distribution {#timeBodyAccelerometerJerk_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometerJerk-std()-X |numeric   |0       |180      |180 |-0.59 |0.42 |-0.99 |-0.98 |-0.81 |-0.22 |0.54 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-std()-Y {#timeBodyAccelerometerJerk_std___Y .tabset}



#### Distribution {#timeBodyAccelerometerJerk_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometerJerk-std()-Y |numeric   |0       |180      |180 |-0.57 |0.43 |-0.99 |-0.97 |-0.78 |-0.15 |0.36 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerk-std()-Z {#timeBodyAccelerometerJerk_std___Z .tabset}



#### Distribution {#timeBodyAccelerometerJerk_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerk_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerk_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                              |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100  |hist     |
|:---------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:-----|:--------|
|timeBodyAccelerometerJerk-std()-Z |numeric   |0       |180      |180 |-0.74 |0.28 |-0.99 |-0.98 |-0.88 |-0.51 |0.031 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-std()-X {#timeBodyGyroscope_std___X .tabset}



#### Distribution {#timeBodyGyroscope_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                      |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscope-std()-X |numeric   |0       |180      |180 |-0.69 |0.29 |-0.99 |-0.97 |-0.79 |-0.44 |0.27 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-std()-Y {#timeBodyGyroscope_std___Y .tabset}



#### Distribution {#timeBodyGyroscope_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                      |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50  |p75   |p100 |hist     |
|:-------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:----|:-----|:----|:--------|
|timeBodyGyroscope-std()-Y |numeric   |0       |180      |180 |-0.65 |0.35 |-0.99 |-0.96 |-0.8 |-0.42 |0.48 |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscope-std()-Z {#timeBodyGyroscope_std___Z .tabset}



#### Distribution {#timeBodyGyroscope_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscope_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscope_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                      |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50  |p75   |p100 |hist     |
|:-------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:----|:-----|:----|:--------|
|timeBodyGyroscope-std()-Z |numeric   |0       |180      |180 |-0.62 |0.37 |-0.99 |-0.96 |-0.8 |-0.31 |0.56 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-std()-X {#timeBodyGyroscopeJerk_std___X .tabset}



#### Distribution {#timeBodyGyroscopeJerk_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean |sd  |p0 |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:----|:---|:--|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeJerk-std()-X |numeric   |0       |180      |180 |-0.7 |0.3 |-1 |-0.98 |-0.84 |-0.46 |0.18 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-std()-Y {#timeBodyGyroscopeJerk_std___Y .tabset}



#### Distribution {#timeBodyGyroscopeJerk_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeJerk-std()-Y |numeric   |0       |180      |180 |-0.76 |0.27 |-1 |-0.98 |-0.89 |-0.59 |0.3  |<U+2587><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerk-std()-Z {#timeBodyGyroscopeJerk_std___Z .tabset}



#### Distribution {#timeBodyGyroscopeJerk_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerk_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerk_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                          |data_type |missing |complete |n   |mean  |sd  |p0 |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------|:---------|:-------|:--------|:---|:-----|:---|:--|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeJerk-std()-Z |numeric   |0       |180      |180 |-0.71 |0.3 |-1 |-0.98 |-0.86 |-0.47 |0.19 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerMagnitude-std() {#timeBodyAccelerometerMagnitude_std__ .tabset}



#### Distribution {#timeBodyAccelerometerMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                 |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyAccelerometerMagnitude-std() |numeric   |0       |180      |180 |-0.54 |0.43 |-0.99 |-0.94 |-0.61 |-0.21 |0.43 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2582><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeGravityAccelerometerMagnitude-std() {#timeGravityAccelerometerMagnitude_std__ .tabset}



#### Distribution {#timeGravityAccelerometerMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeGravityAccelerometerMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeGravityAccelerometerMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                    |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeGravityAccelerometerMagnitude-std() |numeric   |0       |180      |180 |-0.54 |0.43 |-0.99 |-0.94 |-0.61 |-0.21 |0.43 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2582><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyAccelerometerJerkMagnitude-std() {#timeBodyAccelerometerJerkMagnitude_std__ .tabset}



#### Distribution {#timeBodyAccelerometerJerkMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyAccelerometerJerkMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyAccelerometerJerkMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                     |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50  |p75   |p100 |hist     |
|:----------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:----|:-----|:----|:--------|
|timeBodyAccelerometerJerkMagnitude-std() |numeric   |0       |180      |180 |-0.58 |0.42 |-0.99 |-0.98 |-0.8 |-0.22 |0.45 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeMagnitude-std() {#timeBodyGyroscopeMagnitude_std__ .tabset}



#### Distribution {#timeBodyGyroscopeMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                             |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:--------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeMagnitude-std() |numeric   |0       |180      |180 |-0.63 |0.34 |-0.98 |-0.95 |-0.74 |-0.36 |0.3  |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### timeBodyGyroscopeJerkMagnitude-std() {#timeBodyGyroscopeJerkMagnitude_std__ .tabset}



#### Distribution {#timeBodyGyroscopeJerkMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/timeBodyGyroscopeJerkMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#timeBodyGyroscopeJerkMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                 |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100 |hist     |
|:------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:----|:--------|
|timeBodyGyroscopeJerkMagnitude-std() |numeric   |0       |180      |180 |-0.76 |0.27 |-1 |-0.98 |-0.88 |-0.58 |0.25 |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-std()-X {#frequencyBodyAccelerometer_std___X .tabset}



#### Distribution {#frequencyBodyAccelerometer_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75  |p100 |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:----|:----|:--------|
|frequencyBodyAccelerometer-std()-X |numeric   |0       |180      |180 |-0.55 |0.46 |-1 |-0.98 |-0.75 |-0.2 |0.66 |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-std()-Y {#frequencyBodyAccelerometer_std___Y .tabset}



#### Distribution {#frequencyBodyAccelerometer_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75    |p100 |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:------|:----|:--------|
|frequencyBodyAccelerometer-std()-Y |numeric   |0       |180      |180 |-0.48 |0.47 |-0.99 |-0.94 |-0.51 |-0.079 |0.56 |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometer-std()-Z {#frequencyBodyAccelerometer_std___Z .tabset}



#### Distribution {#frequencyBodyAccelerometer_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometer_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometer_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                               |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:----------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometer-std()-Z |numeric   |0       |180      |180 |-0.58 |0.39 |-0.99 |-0.95 |-0.64 |-0.27 |0.69 |<U+2587><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-std()-X {#frequencyBodyAccelerometerJerk_std___X .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                   |data_type |missing |complete |n   |mean  |sd  |p0 |p25   |p50   |p75   |p100 |hist     |
|:--------------------------------------|:---------|:-------|:--------|:---|:-----|:---|:--|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerk-std()-X |numeric   |0       |180      |180 |-0.61 |0.4 |-1 |-0.98 |-0.83 |-0.25 |0.48 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-std()-Y {#frequencyBodyAccelerometerJerk_std___Y .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                   |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:--------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerk-std()-Y |numeric   |0       |180      |180 |-0.57 |0.43 |-0.99 |-0.97 |-0.79 |-0.17 |0.35 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerk-std()-Z {#frequencyBodyAccelerometerJerk_std___Z .tabset}



#### Distribution {#frequencyBodyAccelerometerJerk_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerk_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerk_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                   |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50  |p75   |p100    |hist     |
|:--------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:----|:-----|:-------|:--------|
|frequencyBodyAccelerometerJerk-std()-Z |numeric   |0       |180      |180 |-0.76 |0.26 |-0.99 |-0.98 |-0.9 |-0.54 |-0.0062 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-std()-X {#frequencyBodyGyroscope_std___X .tabset}



#### Distribution {#frequencyBodyGyroscope_std___X_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_std___X_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_std___X_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscope-std()-X |numeric   |0       |180      |180 |-0.71 |0.27 |-0.99 |-0.98 |-0.81 |-0.48 |0.2  |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-std()-Y {#frequencyBodyGyroscope_std___Y .tabset}



#### Distribution {#frequencyBodyGyroscope_std___Y_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_std___Y_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_std___Y_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50  |p75   |p100 |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:----|:-----|:----|:--------|
|frequencyBodyGyroscope-std()-Y |numeric   |0       |180      |180 |-0.65 |0.36 |-0.99 |-0.96 |-0.8 |-0.42 |0.65 |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscope-std()-Z {#frequencyBodyGyroscope_std___Z .tabset}



#### Distribution {#frequencyBodyGyroscope_std___Z_distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscope_std___Z_distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscope_std___Z_summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                           |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscope-std()-Z |numeric   |0       |180      |180 |-0.66 |0.34 |-0.99 |-0.96 |-0.82 |-0.39 |0.52 |<U+2587><U+2581><U+2583><U+2583><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerMagnitude-std() {#frequencyBodyAccelerometerMagnitude_std__ .tabset}



#### Distribution {#frequencyBodyAccelerometerMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                      |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerMagnitude-std() |numeric   |0       |180      |180 |-0.62 |0.35 |-0.99 |-0.95 |-0.65 |-0.37 |0.18 |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyAccelerometerJerkMagnitude-std() {#frequencyBodyAccelerometerJerkMagnitude_std__ .tabset}



#### Distribution {#frequencyBodyAccelerometerJerkMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyAccelerometerJerkMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyAccelerometerJerkMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                          |data_type |missing |complete |n   |mean |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:---------------------------------------------|:---------|:-------|:--------|:---|:----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyAccelerometerJerkMagnitude-std() |numeric   |0       |180      |180 |-0.6 |0.41 |-0.99 |-0.98 |-0.81 |-0.27 |0.32 |<U+2587><U+2581><U+2581><U+2582><U+2581><U+2582><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscopeMagnitude-std() {#frequencyBodyGyroscopeMagnitude_std__ .tabset}



#### Distribution {#frequencyBodyGyroscopeMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscopeMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscopeMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                  |data_type |missing |complete |n   |mean  |sd   |p0    |p25   |p50   |p75   |p100 |hist     |
|:-------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:-----|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscopeMagnitude-std() |numeric   |0       |180      |180 |-0.67 |0.29 |-0.98 |-0.95 |-0.77 |-0.43 |0.24 |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```









### frequencyBodyGyroscopeJerkMagnitude-std() {#frequencyBodyGyroscopeJerkMagnitude_std__ .tabset}



#### Distribution {#frequencyBodyGyroscopeJerkMagnitude_std___distribution}

```r
show_missing_values <- FALSE
if (has_labels(item)) {
  missing_values <- item[is.na(haven::zap_missing(item))]
  attributes(missing_values) <- attributes(item)
  if (!is.null(attributes(item)$labels)) {
    attributes(missing_values)$labels <- attributes(missing_values)$labels[is.na(attributes(missing_values)$labels)]
    attributes(item)$labels <- attributes(item)$labels[!is.na(attributes(item)$labels)]
  }
  if (is.double(item)) {
    show_missing_values <- length(unique(haven::na_tag(missing_values))) > 1
    item <- haven::zap_missing(item)
  }
  if (length(item_attributes$labels) == 0 && is.numeric(item)) {
    item <- haven::zap_labels(item)
  }
}
item_nomiss <- item[!is.na(item)]

# unnest mc_multiple and so on
if (
  is.character(item_nomiss) &&
  any(stringr::str_detect(item_nomiss, stringr::fixed(", "))) &&
  (exists("type", item_info) && 
    any(stringr::str_detect(item_info$type, 
                            pattern = stringr::fixed("multiple"))))
  ) {
  item_nomiss <- unlist(stringr::str_split(item_nomiss, pattern = stringr::fixed(", ")))
}
attributes(item_nomiss) <- attributes(item)

old_height <- knitr::opts_chunk$get("fig.height")
non_missing_choices <- item_attributes[["labels"]]
many_labels <- length(non_missing_choices) > 7
go_vertical <- !is.numeric(item_nomiss) || many_labels
if ( go_vertical ) {
  # numeric items are plotted horizontally (because that's what usually expected)
  # categorical items are plotted vertically because we can use the screen real estate better this way

	if (is.null(choices) || 
	    dplyr::n_distinct(item_nomiss) > length(non_missing_choices)) {
		non_missing_choices <- unique(item_nomiss)
		names(non_missing_choices) <- non_missing_choices
	}
  choice_multiplier <- old_height/6.5
	new_height <- 2 + choice_multiplier * length(non_missing_choices)
	new_height <- ifelse(new_height > 20, 20, new_height)
	new_height <- ifelse(new_height < 1, 1, new_height)
	knitr::opts_chunk$set(fig.height = new_height)
}

wrap_at <- knitr::opts_chunk$get("fig.width") * 10
```

```r
# todo: if there are free-text choices mingled in with the pre-defined ones, don't show
# todo: show rare items if they are pre-defined
# todo: bin rare responses into "other category"
if (!length(item_nomiss)) {
  cat("No non-missing values to show.")
} else if (is.numeric(item_nomiss) || dplyr::n_distinct(item_nomiss) < 20) {
  plot_labelled(item_nomiss, item_name, wrap_at, go_vertical)
} else {
	cat(dplyr::n_distinct(item_nomiss), " unique, categorical values, so not shown.")
}
```

![plot of chunk distribution](figure/frequencyBodyGyroscopeJerkMagnitude_std___distribution-1.png)

```r
knitr::opts_chunk$set(fig.height = old_height)
```

0 missing values.

#### Summary statistics {#frequencyBodyGyroscopeJerkMagnitude_std___summary}

```r
attributes(item) <- item_attributes
df = data.frame(item, stringsAsFactors = FALSE)
names(df) = html_item_name
escaped_table(codebook_table(df))
```



|name                                      |data_type |missing |complete |n   |mean  |sd   |p0 |p25   |p50   |p75   |p100 |hist     |
|:-----------------------------------------|:---------|:-------|:--------|:---|:-----|:----|:--|:-----|:-----|:-----|:----|:--------|
|frequencyBodyGyroscopeJerkMagnitude-std() |numeric   |0       |180      |180 |-0.77 |0.25 |-1 |-0.98 |-0.89 |-0.61 |0.29 |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |

```r
if (show_missing_values) {
  plot_labelled(missing_values, item_name, wrap_at)
}
```

```r
if (!is.null(item_info)) {
  # don't show choices again, if they're basically same thing as value labels
  if (!is.null(choices) && !is.null(item_info$choices) && 
    all(names(na.omit(choices)) == item_info$choices) &&
    all(na.omit(choices) == names(item_info$choices))) {
    item_info$choices <- NULL
  }
  item_info$label_parsed <- 
    item_info$choice_list <- item_info$study_id <- item_info$id <- NULL
  pander::pander(item_info)
}
```

```r
if (!is.null(choices) && length(choices) && length(choices) < 30) {
	pander::pander(as.list(choices))
}
```


```r
missingness_report
```





## Missingness report

Among those who finished the survey. Only variables that have missing values are shown.

```r
if (!exists("ended", results) ||
  !exists("expired", results)) {
  warning("Could not figure out who finished the surveys, because the ",
          "variables expired and ended were missing.")
}
```

```
## Warning: Could not figure out who finished the surveys, because the
## variables expired and ended were missing.
```

```r
if (length(md_pattern)) {
  if (knitr::is_html_output()) {
    rmarkdown::paged_table(md_pattern, options = list(rows.print = 10))
  } else {
    knitr::kable(md_pattern)
  }
}
```



```r
items
```




## Codebook table

```r
export_table(metadata_table)
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```


```r
jsonld
```




<script type="application/ld+json">
{
  "name": "tidy_data",
  "datePublished": "2019-01-01",
  "description": "The dataset has N=180 rows and 68 columns.\n180 rows have no missing values on any column.\n\n\n## Table of variables\nThis table contains variable names, labels, their central tendencies and other attributes.\n\n|name                                           |data_type |missing |complete |n   |empty |n_unique |min |max |mean   |sd     |p0     |p25     |p50     |p75    |p100    |hist     |\n|:----------------------------------------------|:---------|:-------|:--------|:---|:-----|:--------|:---|:---|:------|:------|:------|:-------|:-------|:------|:-------|:--------|\n|volunteer                                      |integer   |0       |180      |180 |NA    |NA       |NA  |NA  |15.5   |8.68   |1      |8       |15.5    |23     |30      |<U+2587><U+2587><U+2586><U+2587><U+2587><U+2586><U+2587><U+2587> |\n|activity                                       |character |0       |180      |180 |0     |6        |6   |18  |NA     |NA     |NA     |NA      |NA      |NA     |NA      |NA       |\n|timeBodyAccelerometer-mean()-X                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.27   |0.012  |0.22   |0.27    |0.28    |0.28   |0.3     |<U+2581><U+2581><U+2581><U+2581><U+2582><U+2587><U+2582><U+2581> |\n|timeBodyAccelerometer-mean()-Y                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.018 |0.0058 |-0.041 |-0.02   |-0.017  |-0.015 |-0.0013 |<U+2581><U+2581><U+2582><U+2582><U+2587><U+2583><U+2581><U+2581> |\n|timeBodyAccelerometer-mean()-Z                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.11  |0.0096 |-0.15  |-0.11   |-0.11   |-0.1   |-0.075  |<U+2581><U+2581><U+2581><U+2582><U+2587><U+2583><U+2581><U+2581> |\n|timeGravityAccelerometer-mean()-X              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.7    |0.49   |-0.68  |0.84    |0.92    |0.94   |0.97    |<U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581><U+2587> |\n|timeGravityAccelerometer-mean()-Y              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.016 |0.35   |-0.48  |-0.23   |-0.13   |0.088  |0.96    |<U+2582><U+2587><U+2585><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|timeGravityAccelerometer-mean()-Z              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.074  |0.29   |-0.5   |-0.12   |0.024   |0.15   |0.96    |<U+2581><U+2585><U+2587><U+2586><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-mean()-X             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.079  |0.013  |0.043  |0.074   |0.076   |0.083  |0.13    |<U+2581><U+2581><U+2587><U+2587><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-mean()-Y             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.0076 |0.014  |-0.039 |0.00047 |0.0095  |0.013  |0.057   |<U+2581><U+2581><U+2582><U+2586><U+2587><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-mean()-Z             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.005 |0.013  |-0.067 |-0.011  |-0.0039 |0.002  |0.038   |<U+2581><U+2581><U+2581><U+2581><U+2587><U+2586><U+2581><U+2581> |\n|timeBodyGyroscope-mean()-X                     |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.032 |0.054  |-0.21  |-0.047  |-0.029  |-0.017 |0.19    |<U+2581><U+2581><U+2582><U+2587><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyGyroscope-mean()-Y                     |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.074 |0.036  |-0.2   |-0.09   |-0.073  |-0.061 |0.027   |<U+2581><U+2581><U+2581><U+2583><U+2587><U+2582><U+2581><U+2581> |\n|timeBodyGyroscope-mean()-Z                     |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |0.087  |0.036  |-0.072 |0.075   |0.085   |0.1    |0.18    |<U+2581><U+2581><U+2581><U+2581><U+2587><U+2587><U+2582><U+2581> |\n|timeBodyGyroscopeJerk-mean()-X                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.096 |0.023  |-0.16  |-0.1    |-0.099  |-0.091 |-0.022  |<U+2581><U+2581><U+2581><U+2587><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerk-mean()-Y                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.043 |0.0095 |-0.077 |-0.046  |-0.041  |-0.038 |-0.013  |<U+2581><U+2581><U+2581><U+2582><U+2587><U+2582><U+2581><U+2581> |\n|timeBodyGyroscopeJerk-mean()-Z                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.055 |0.012  |-0.092 |-0.062  |-0.053  |-0.049 |-0.0069 |<U+2581><U+2581><U+2583><U+2587><U+2583><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometerMagnitude-mean()          |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.5   |0.47   |-0.99  |-0.96   |-0.48   |-0.092 |0.64    |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581> |\n|timeGravityAccelerometerMagnitude-mean()       |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.5   |0.47   |-0.99  |-0.96   |-0.48   |-0.092 |0.64    |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometerJerkMagnitude-mean()      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.61  |0.4    |-0.99  |-0.98   |-0.82   |-0.25  |0.43    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeMagnitude-mean()              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.57  |0.4    |-0.98  |-0.95   |-0.66   |-0.22  |0.42    |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerkMagnitude-mean()          |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.74  |0.28   |-1     |-0.99   |-0.86   |-0.51  |0.088   |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometer-mean()-X            |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.58  |0.43   |-1     |-0.98   |-0.77   |-0.22  |0.54    |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometer-mean()-Y            |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.49  |0.48   |-0.99  |-0.95   |-0.59   |-0.063 |0.52    |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometer-mean()-Z            |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.63  |0.36   |-0.99  |-0.96   |-0.72   |-0.32  |0.28    |<U+2587><U+2581><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-mean()-X        |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.61  |0.4    |-0.99  |-0.98   |-0.81   |-0.28  |0.47    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-mean()-Y        |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.59  |0.41   |-0.99  |-0.97   |-0.78   |-0.2   |0.28    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-mean()-Z        |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.71  |0.3    |-0.99  |-0.98   |-0.87   |-0.47  |0.16    |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-mean()-X                |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.64  |0.35   |-0.99  |-0.97   |-0.73   |-0.34  |0.47    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-mean()-Y                |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.68  |0.33   |-0.99  |-0.97   |-0.81   |-0.45  |0.33    |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-mean()-Z                |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.6   |0.38   |-0.99  |-0.96   |-0.79   |-0.26  |0.49    |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerMagnitude-mean()     |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.54  |0.45   |-0.99  |-0.96   |-0.67   |-0.16  |0.59    |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerkMagnitude-mean() |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.58  |0.43   |-0.99  |-0.98   |-0.79   |-0.19  |0.54    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyGyroscopeMagnitude-mean()         |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.67  |0.32   |-0.99  |-0.96   |-0.77   |-0.41  |0.2     |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscopeJerkMagnitude-mean()     |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.76  |0.26   |-1     |-0.98   |-0.88   |-0.58  |0.15    |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometer-std()-X                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.56  |0.45   |-1     |-0.98   |-0.75   |-0.2   |0.63    |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometer-std()-Y                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.46  |0.5    |-0.99  |-0.94   |-0.51   |-0.031 |0.62    |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometer-std()-Z                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.58  |0.4    |-0.99  |-0.95   |-0.65   |-0.23  |0.61    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|timeGravityAccelerometer-std()-X               |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.96  |0.025  |-1     |-0.98   |-0.97   |-0.95  |-0.83   |<U+2587><U+2586><U+2585><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|timeGravityAccelerometer-std()-Y               |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.95  |0.033  |-0.99  |-0.97   |-0.96   |-0.94  |-0.64   |<U+2587><U+2585><U+2581><U+2581><U+2581><U+2581><U+2581><U+2581> |\n|timeGravityAccelerometer-std()-Z               |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.94  |0.04   |-0.99  |-0.96   |-0.95   |-0.92  |-0.61   |<U+2587><U+2586><U+2582><U+2581><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-std()-X              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.59  |0.42   |-0.99  |-0.98   |-0.81   |-0.22  |0.54    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-std()-Y              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.57  |0.43   |-0.99  |-0.97   |-0.78   |-0.15  |0.36    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|timeBodyAccelerometerJerk-std()-Z              |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.74  |0.28   |-0.99  |-0.98   |-0.88   |-0.51  |0.031   |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscope-std()-X                      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.69  |0.29   |-0.99  |-0.97   |-0.79   |-0.44  |0.27    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscope-std()-Y                      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.65  |0.35   |-0.99  |-0.96   |-0.8    |-0.42  |0.48    |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyGyroscope-std()-Z                      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.62  |0.37   |-0.99  |-0.96   |-0.8    |-0.31  |0.56    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerk-std()-X                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.7   |0.3    |-1     |-0.98   |-0.84   |-0.46  |0.18    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerk-std()-Y                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.76  |0.27   |-1     |-0.98   |-0.89   |-0.59  |0.3     |<U+2587><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerk-std()-Z                  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.71  |0.3    |-1     |-0.98   |-0.86   |-0.47  |0.19    |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyAccelerometerMagnitude-std()           |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.54  |0.43   |-0.99  |-0.94   |-0.61   |-0.21  |0.43    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2582><U+2581> |\n|timeGravityAccelerometerMagnitude-std()        |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.54  |0.43   |-0.99  |-0.94   |-0.61   |-0.21  |0.43    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2582><U+2581> |\n|timeBodyAccelerometerJerkMagnitude-std()       |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.58  |0.42   |-0.99  |-0.98   |-0.8    |-0.22  |0.45    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|timeBodyGyroscopeMagnitude-std()               |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.63  |0.34   |-0.98  |-0.95   |-0.74   |-0.36  |0.3     |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|timeBodyGyroscopeJerkMagnitude-std()           |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.76  |0.27   |-1     |-0.98   |-0.88   |-0.58  |0.25    |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometer-std()-X             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.55  |0.46   |-1     |-0.98   |-0.75   |-0.2   |0.66    |<U+2587><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometer-std()-Y             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.48  |0.47   |-0.99  |-0.94   |-0.51   |-0.079 |0.56    |<U+2587><U+2581><U+2581><U+2581><U+2583><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometer-std()-Z             |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.58  |0.39   |-0.99  |-0.95   |-0.64   |-0.27  |0.69    |<U+2587><U+2581><U+2582><U+2583><U+2581><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-std()-X         |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.61  |0.4    |-1     |-0.98   |-0.83   |-0.25  |0.48    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-std()-Y         |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.57  |0.43   |-0.99  |-0.97   |-0.79   |-0.17  |0.35    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerk-std()-Z         |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.76  |0.26   |-0.99  |-0.98   |-0.9    |-0.54  |-0.0062 |<U+2587><U+2581><U+2582><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-std()-X                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.71  |0.27   |-0.99  |-0.98   |-0.81   |-0.48  |0.2     |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-std()-Y                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.65  |0.36   |-0.99  |-0.96   |-0.8    |-0.42  |0.65    |<U+2587><U+2581><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscope-std()-Z                 |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.66  |0.34   |-0.99  |-0.96   |-0.82   |-0.39  |0.52    |<U+2587><U+2581><U+2583><U+2583><U+2581><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerMagnitude-std()      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.62  |0.35   |-0.99  |-0.95   |-0.65   |-0.37  |0.18    |<U+2587><U+2581><U+2581><U+2582><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyAccelerometerJerkMagnitude-std()  |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.6   |0.41   |-0.99  |-0.98   |-0.81   |-0.27  |0.32    |<U+2587><U+2581><U+2581><U+2582><U+2581><U+2582><U+2581><U+2581> |\n|frequencyBodyGyroscopeMagnitude-std()          |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.67  |0.29   |-0.98  |-0.95   |-0.77   |-0.43  |0.24    |<U+2587><U+2581><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581> |\n|frequencyBodyGyroscopeJerkMagnitude-std()      |numeric   |0       |180      |180 |NA    |NA       |NA  |NA  |-0.77  |0.25   |-1     |-0.98   |-0.89   |-0.61  |0.29    |<U+2587><U+2582><U+2583><U+2582><U+2581><U+2581><U+2581><U+2581> |\n\n### Note\nThis dataset was automatically described using the [codebook R package](https://rubenarslan.github.io/codebook/).",
  "keywords": ["volunteer", "activity", "timeBodyAccelerometer-mean()-X", "timeBodyAccelerometer-mean()-Y", "timeBodyAccelerometer-mean()-Z", "timeGravityAccelerometer-mean()-X", "timeGravityAccelerometer-mean()-Y", "timeGravityAccelerometer-mean()-Z", "timeBodyAccelerometerJerk-mean()-X", "timeBodyAccelerometerJerk-mean()-Y", "timeBodyAccelerometerJerk-mean()-Z", "timeBodyGyroscope-mean()-X", "timeBodyGyroscope-mean()-Y", "timeBodyGyroscope-mean()-Z", "timeBodyGyroscopeJerk-mean()-X", "timeBodyGyroscopeJerk-mean()-Y", "timeBodyGyroscopeJerk-mean()-Z", "timeBodyAccelerometerMagnitude-mean()", "timeGravityAccelerometerMagnitude-mean()", "timeBodyAccelerometerJerkMagnitude-mean()", "timeBodyGyroscopeMagnitude-mean()", "timeBodyGyroscopeJerkMagnitude-mean()", "frequencyBodyAccelerometer-mean()-X", "frequencyBodyAccelerometer-mean()-Y", "frequencyBodyAccelerometer-mean()-Z", "frequencyBodyAccelerometerJerk-mean()-X", "frequencyBodyAccelerometerJerk-mean()-Y", "frequencyBodyAccelerometerJerk-mean()-Z", "frequencyBodyGyroscope-mean()-X", "frequencyBodyGyroscope-mean()-Y", "frequencyBodyGyroscope-mean()-Z", "frequencyBodyAccelerometerMagnitude-mean()", "frequencyBodyAccelerometerJerkMagnitude-mean()", "frequencyBodyGyroscopeMagnitude-mean()", "frequencyBodyGyroscopeJerkMagnitude-mean()", "timeBodyAccelerometer-std()-X", "timeBodyAccelerometer-std()-Y", "timeBodyAccelerometer-std()-Z", "timeGravityAccelerometer-std()-X", "timeGravityAccelerometer-std()-Y", "timeGravityAccelerometer-std()-Z", "timeBodyAccelerometerJerk-std()-X", "timeBodyAccelerometerJerk-std()-Y", "timeBodyAccelerometerJerk-std()-Z", "timeBodyGyroscope-std()-X", "timeBodyGyroscope-std()-Y", "timeBodyGyroscope-std()-Z", "timeBodyGyroscopeJerk-std()-X", "timeBodyGyroscopeJerk-std()-Y", "timeBodyGyroscopeJerk-std()-Z", "timeBodyAccelerometerMagnitude-std()", "timeGravityAccelerometerMagnitude-std()", "timeBodyAccelerometerJerkMagnitude-std()", "timeBodyGyroscopeMagnitude-std()", "timeBodyGyroscopeJerkMagnitude-std()", "frequencyBodyAccelerometer-std()-X", "frequencyBodyAccelerometer-std()-Y", "frequencyBodyAccelerometer-std()-Z", "frequencyBodyAccelerometerJerk-std()-X", "frequencyBodyAccelerometerJerk-std()-Y", "frequencyBodyAccelerometerJerk-std()-Z", "frequencyBodyGyroscope-std()-X", "frequencyBodyGyroscope-std()-Y", "frequencyBodyGyroscope-std()-Z", "frequencyBodyAccelerometerMagnitude-std()", "frequencyBodyAccelerometerJerkMagnitude-std()", "frequencyBodyGyroscopeMagnitude-std()", "frequencyBodyGyroscopeJerkMagnitude-std()"],
  "@context": "http://schema.org/",
  "@type": "Dataset",
  "variableMeasured": [
    {
      "name": "volunteer",
      "@type": "propertyValue"
    },
    {
      "name": "activity",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometerMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerkMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerkMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-mean()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-mean()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-mean()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerkMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscopeMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscopeJerkMagnitude-mean()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometer-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometer-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerk-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscope-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerk-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "timeGravityAccelerometerMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyAccelerometerJerkMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "timeBodyGyroscopeJerkMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometer-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerk-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-std()-X",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-std()-Y",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscope-std()-Z",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyAccelerometerJerkMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscopeMagnitude-std()",
      "@type": "propertyValue"
    },
    {
      "name": "frequencyBodyGyroscopeJerkMagnitude-std()",
      "@type": "propertyValue"
    }
  ]
}
</script>
