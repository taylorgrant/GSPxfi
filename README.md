
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

There are certain LOBs that only apply to certain platforms. This
function accounts for that. It is also able to work around empty
matrices.

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

All averages are based on the total creative out from all agencies, but
the creative data is filtered down to only GS&P.

``` r
meta_sum <- merged_summarise(merged_meta, group_vars = c("platform", "ad_media_type", "platform_position", "ad_objective", "creative_detail"))
```
