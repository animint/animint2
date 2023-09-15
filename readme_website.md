# Maintaining & Debugging the Animint2 Website

## About

First written 2023-06-28. Last revised 2023-08-22.

This is an internal document intended for `animint2` contributors. It explains how to maintain and debug the `animint2` reference website and gives some reasons why you might need to. Feel free to add to and edit this document.

Maintaining the site is neither hard nor annoying, but it's also not automatic. Most of the time, you can leave the website alone. But there are some circumstances when you would need to maintain or update the website:

- You added a new function to `animint2`.
- You're getting errors about the GitHub access token or the PAT.
- You made changes to the quick start guide.
- You wanna change the reference website's URL.
- You wanna change the reference website's typeface or font.
- The website is messed up or has stopped working in some way.
- You made changes to some file in the `animint2` repository and it should have some effect on the website. Examples include `NEWS.md`, `_pkgdown.yml`, or `pkgdown/extra.css`.

## Have You Tried Re-Building It?

You can solve a lot of maintenance problems by running the following:

``` r
cd whatever/your/path/is/animint/animint2
pkgdown::build_site()
```

This takes a few minutes and re-builds the site. Then push the changes to the `animint2` repository.

For a lot of problems, that's it. But if (substantial) changes aren't showing up when they should, or if this doesn't fix the bug, you'll need to read on.

## Adding a Function

You added a new function to the `animint2` package. Congratulations! It was probably a lot of work. But now you need it to show up on the website. The process is straightforward.

First, do everything in the "Have You Tried Re-Building It?" section. If your new function starts with "animint," "make," or "get," your new function will appear on the website. No further work needed.

If it doesn't, probably because it doesn't start with any of those prefixes, you'll have to do more work. First, open `_pkgdown.yml`, scroll down to `references:`, and find the section where your new function should be categorized. My guess is that it'd fall under

``` yaml
- subtitle: "Animint Helpers"
```

but you'd know best. Next, slot it under `- contents:`. Then run the code in the "Have You Tried Re-Building It?" section again and push the changes to the `animint2` repo.

## Refreshing the GitHub Access Token

If you're getting errors about GitHub access tokens or PATs, that means that they have either expired, changed, or been destroyed, run

``` r
cd whatever/your/path/is/animint/animint2
usethis::create_github_token()
gitcreds::gitcreds_set()
```

and follow the instructions.

## Changes to the Quick Start Guide

First, do everything in the "Have You Tried Re-Building It?" section. If your new content hasn't been added, it's probably because you used `animint2`, which renders animints in `vignettes/` instead of `docs/`. You'll need to cut/paste the rendered directories and drop them in `docs/articles/`. Then run the code in the "Have You Tried Re-Building It?" section again and push the changes to the `animint2` repo.

It might be possible to use `animint2::animint2dir()` to automate this. Unfortunately, I haven't figured out a way to use it in a way that's more elegant and maintainable than the cut/paste solution. If you figure it out, feel free to implement it.

## Changes to the URL

If you wanna change the URL: go to `_pkgdown.yml`, edit the `url:` or add another, and then do everything in the "Have You Tried Re-Building It?" section.

## Changes to the Typeface or Font

If you wanna change the typeface (often referred to as the font): go to `_pkgdown.yml`, then `bslib:`. You'll see:

- `base_font`, which controls the default typeface,
- `heading_font`, which controls the typeface for titles and headings, and
- `code_font`, which controls the font in the codeblocks and in in-line code.

Swap out the current font with ones of your choice, and then do everything in the "Have You Tried Re-Building It?" section.

## The Website is Messed Up

Is the website messed up after you pushed some commits to the `animint2` repository? Revert your commits with `git revert $SHA` or something. Then debug to find out what happened.

If the website is messed up cuz of changes you made via the GitHub API or via the GitHub website, you'll need to figure out exactly what you changed and how that's affect the website. Good luck.

## Changes to Other Animint2 Files

If a change to the repository is meant to affect `animint2`'s CRAN page, then you'll also need to update the website, too. Do everything in the "Have You Tried Re-Building It?" section.

Some changes to the repository are only meant to affect `animint2`'s reference website. If that's the case, do everything in the "Have You Tried Re-Building It?" section.

## Why Don't You Just Use GitHub Actions?

Using GitHub Actions would automate many parts of maintaining the site, though it'd leave some parts untouched (e.g. refreshing the access token). The main reason I haven't done it is cuz I dunno how! Feel free to set up the site via GitHub Actions. That'd be great.

## See Also

Documentation that may be helpful:

- [Managing GitHub Credentials](https://usethis.r-lib.org/articles/git-credentials.html)
- [`pkgdown` Documentation](https://pkgdown.r-lib.org/index.html)
- [Information on `usethis::use_pkgdown`](https://usethis.r-lib.org/reference/use_pkgdown.html)
- [GitHub-flavored Markdown Specification](https://github.github.com/gfm/)
