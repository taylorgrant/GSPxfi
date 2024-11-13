
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GSPxfi

<!-- badges: start -->
<!-- badges: end -->

GSPxfi is a utility package that pulls in data from IAT Social Matrices,
merges it with Sprinklr data, and then summarizes media KPIs for easy
analysis.

## Installation

You can install the development version of GSPxfi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("taylorgrant/GSPxfi")
```

## Set Up

In order to read in the social matrices master list, the user has to add
the master list ID to a .Renviron file – `XFI_MATRIX_ID=[MASTER_ID]`.
After adding this, restart the session.

The user also has to authorize their email address using the
`googlesheets4::gs4_auth(email = email_address)`. This email address
needs to be granted permission to access all sheets and workbooks by the
media company.

It is recommended that the user create a new project. Within the
project, create a /data/ folder to put Sprinklr data.

## Functionality

### Pulling master index

The `full_range` variable is the column that contains the links with the
last row of data. In this case, all links are in column B and there are
43 rows.

``` r
master_ids <- get_matrix_ids(sheet = 3, full_range = 'B43', email = "email_address")
```

Since the links to the `master_ids` only change once a quarter, it’s
recommended that the user save the ids to the /data/ folder.

### Reading in Sprinklr data

After downloading Sprinklr data, it needs to be read in. The function
below will read it in and clean some of the variables to make the merge
more efficient.

``` r
meta <- read_sprinklr("/location/of/file/sprinklr_data.xlsx")
```

### Get Social Matrix data

The Social Matrices are organized by LOB and social platform. This
function will pull in all data from each matrix - this includes some
hidden sheets.

**LOB/Line:** Central, Internet/Product Diff, NED, Retargeting, West,
Xfinity Mobile

**Platforms:** LinkedIn, Meta, Pinterest, Reddit, Snapchat, TikTok,
Twitter

``` r
matrix_data <- get_matrix_data(master_ids = master_ids, line = "Central", platform = "Meta", email = "email_address")
```

### Merge Social Matrix with Sprinklr data

Now that we have our two datasets, we have to merge them together. Note
that “line” from `get_matrix_data()` and “lob” in `xfi_merge()` should
match each other.

``` r
merged <- xfi_merge(sprinklr_data = meta, matrix_data = matrix_data, lob = "Central")
```

### Merge all LOBs for a platform

The above functions work by platform and LOB. But if the user wants to
merge all LOBs with a complete Sprinklr data set, there is a wrapper
function that will do that.

The user should have to have the `matrix_data` for this function to run.
This way the user has the ability to run more than quarter of data at a
time. This is especially useful if certain pieces of creative run for
longer time periods. There are certain LOBs that only apply to certain
platforms. This function accounts for that. It is also able to work
around empty matrices.

``` r
merged_meta <- merge_all_lobs(platform = 'Meta', email_address = "email_address", sprinklr_data = meta)
```

### Summarizing the media KPIs

With merged data, the user can then summarize the media KPIs by
specifying the grouping variables that should be used. For example, the
below splits out by platform (IG/Facebook), the media type
(Static/Video), the placement (e.g., Feed, Story, Reels, etc), the ad
objective that was bought (awareness, traffic, etc), and the creative
name.

The returned data also returns average performance for each grouping -
the averages are split out by Video and Static where appropriate (for
example, TikTok is only video, so there are no static averages).

All averages are based on the total creative output from all agencies,
but the creative data is filtered down to only GS&P.

``` r
meta_sum <- merged_summarise(merged_meta, group_vars = c("platform", "ad_media_type", "platform_position", "ad_objective", "creative_detail"))
```

### Estimating creative decay / creative wearout

If the platform data from Sprinklr is at the daily level, then we can
estimate creative wearout using a simple wrapper function -
`decay_build()`. The function takes the merged dataset, and aggregates
the data down using the `merged_summarise()` function based upon the
`group_vars` passed through as an argument. *Note - one of the
group_vars must be “date”, without the date, the function can’t estimate
the daily decay.* It then estimates the daily exponential and
logarithmic decay of the creative based upon a specific KPI. At this
point, because social creative tends to not run for extended periods of
time there isn’t much use in aggregating to the weekly level.

The `decay_build()` function takes the following as arguments - the
merged daily data for a specific platform (e.g., “merged_meta” from
above), the group_vars to group by (including date), and the KPI of
interest (the KPI is the DV in the regression models). There are a
number of DVs that are created via the `merged_summarise()` function -
to see them all reference the code for `merged_summarise()`.

As an example, let’s assume that I’ve merged the Sprinklr and matrix
data from Tiktok. Estimating creative wearout is simple. Here we want to
group by the platform, the ad objective, the creative detail (the
creative name), and the date. The KPI of interest is the average
duration of watchtime per video:

``` r
decay_tiktok <- decay_build(merged_tiktok, group_vars = c("platform", "ad_objective", "creative_detail", "date"), dv = "avg_duration")
```

The function returns a named list with two objects - “plot_list” (the
daily data for reference and for plotting), and “results_table” (a
dataframe with the decay rates for each piece of creative). Each
creative has two rows, the first row has the results for the exponential
model, the second for the logarithmic model. If the decay rate is both
negative and statistically significant, the column “significant” will
say TRUE, otherwise it’s FALSE.

#### Plotting the predicted decay versus actuals

If a piece of creative has a significant decay, then we can visualize
it. The “results_table” provides an ID number for each piece of
creative. We use that, along with the “plot_list” data to easily plot
the results. Note that we have to specify the same DV again.

``` r
fit_decay_models(decay_tiktok$plot_list[[22]], "avg_duration", day = "day", plot = TRUE)
```
