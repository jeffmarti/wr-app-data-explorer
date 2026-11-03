# wr-app-data-explorer

Shiny app showing **how long active New and Change water right applications have been
waiting** in WA Ecology's Water Rights Tracking System (WRTS). Data refreshes every
Monday morning via GitHub Actions, which then redeploys the app to its existing
Posit Connect Cloud content item.

Companion app: [wr-applications](https://github.com/jeffmarti/wr-applications), which
maps the same applications. Both use the same WRTS query, so `pipeline/fetch_wrts_export.R`
is a copy of that repo's script; if Ecology changes its search page, fix it in both.

## Why this deploys differently from wr-applications

wr-applications uses Connect Cloud's own "publish from GitHub" (republish on push).
That always creates a **new** content item with a new URL. This app instead keeps its
existing content ID, `01a12284-53a7-e128-5384-1df2201047c9`, because the old
shinyapps.io address redirects to it:

```
waterwater.shinyapps.io/wr-app-data
  -> 01a12284-53a7-e128-5384-1df2201047c9.share.connect.posit.cloud
```

So the GitHub workflow deploys with `rsconnect::deployApp(appId = ...)`, the same
thing the RStudio Publish button does, using Connect Cloud client credentials.
**Don't** also set the app up as a GitHub-published item in Connect Cloud.

## Folder layout

```
wr-app-data-explorer/                 <- RStudio project folder = GitHub repo
├── app.R                             the Shiny app                    (deploy)
├── data/wr_data.rds                  made by the pipeline             (deploy)
├── wria_boundaries.geojson           WRIA polygons                     (deploy)
├── county_boundaries.geojson         county polygons                   (deploy)
├── www/wa_water_rights.svg           banner icon                       (deploy)
├── pipeline/
│   ├── run_pipeline.R                runs the two steps below in order
│   ├── fetch_wrts_export.R           1. query WRTS + download .xlsx  -> pipeline/raw/
│   └── prepare_app_data.R            2. build data/wr_data.rds
├── .github/workflows/refresh-data.yml   weekly refresh + deploy (Mon 15:17 UTC)
├── .gitignore                        keeps raw/, .Renviron, old .xlsx out of GitHub
└── wr-app-data-explorer.Rproj
```

## One-time setup

1. **Create the repo** `jeffmarti/wr-app-data-explorer` on GitHub and push this folder.
2. **Create Connect Cloud credentials**: <https://login.posit.cloud/identity/credentials>
   > New Credentials, "Use with" = **Connect Cloud**. Copy the client ID and secret.
3. **Add them as GitHub secrets** (repo Settings > Secrets and variables > Actions >
   New repository secret): `RSCLOUD_CLIENT_ID` and `RSCLOUD_CLIENT_SECRET`.
4. **Check the app's Variables** on Connect Cloud include `WA_SOCRATA_TOKEN`
   (used by the Processing Times tab). They carry over between deploys.
5. **Test**: GitHub > Actions > "Refresh and deploy waiting-times app" > Run workflow.
   Then open the old shinyapps.io link and check the "Data current as of" date.

## Weekly routine

Nothing, unless the Monday run fails (GitHub emails you). Edits to `app.R` pushed to
`main` deploy automatically too. Before editing locally, always `git pull` first,
because the workflow commits every week.

Publishing from RStudio still works (it uses the same content ID from
`rsconnect/`), but then the live app runs whatever is on your computer until the
next workflow run.
