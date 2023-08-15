---
title: "How was this Website Set Up?"
date: First written 2023-06-28. Last revised 2023-08-14.
---

This is an internal document intended for `animint2` contributors. It documents how I set up the reference website, as well as the code I ran to do so. There is also a section on maintaining the website.

I've tried to make this document and the website-generating procedures clear and near-algorithmic as I reasonably can. Inevitably, some of this may seem condescending. Sorry if it is.


## Get Ready

First, set your working directory to the `animint2` repository. If you're using an command line terminal, type in `cd whatever/your/path/is/animint/animint2`. If you're using RStudio, you can instead use the <kbd>CTRL</kbd><kbd>SHIFT</kbd><kbd>H</kbd> keyboard shortcut and select the `animint2` repository from there.

A `README.md` document should already exist. If it doesn't, create one. Remember to use [GitHub-flavored Markdown](https://github.github.com/gfm/). The `README` should have, at minimum:

1. A section that briefly summarizes what the package is about,
2. A section that teaches the reader how to install both the CRAN and development versions of the package, and
3. A section that briefly demonstrates how to do basic things with the package or links to another page that does.

In my `README`, I included About section with a GIF of someone using `animint2`, an Installation section, a Use section that linked to a quick start guide, a Similar Packages section about other packages that created interactive data visualizations, and a Problems? section asking users to report bugs if they find any.

Next, generate a GitHub token. It's like a more secure password. We use it to access GitHub's API, which we need to set up the website:

```{r, eval = FALSE}
usethis::create_github_token()
```

This function will open a GitHub webpage. Log in and generate the token. The token will be a series of characters. Save that somewhere, at least temporarily.

Run the following and enter the token when asked:

```{r, eval = FALSE}
gitcreds::gitcreds_set()
```

Finally, configure the repository to use `pkgdown`:

```{r, eval = FALSE}
usethis::use_pkgdown()
```

That's it. Now we can build the site.


## Build the Site

We can build the whole site with just a single line of code:

```{r, eval = FALSE}
pkgdown::build_site()
```

This generates HTML files from our `.Rd` files.

With another line of code, we create and edit the `_pkgdown.yml` file, which influences how the site looks:

```{r, eval = FALSE}
usethis::use_pkgdown_github_pages()
```

If there's already a `_pkgdown.yml` file properly set up, just say no when `usethis` offers to modify it.

And you're done with site building.


## Organizing the Site

By default, the home page is just a mirror image of what's on the `animint2` repository, and the references are in alphabetical order. Not great. We can make it better.

We can [rearrange the reference pages](https://pkgdown.r-lib.org/reference/build_reference.html) by adding a `reference` section to the `_pkgdown.yml`. I've linked to the `pkgdown` documentation, which describes the syntax in detail. It's gonna take a while to manually construct, partly due to the sheer number of functions `animint2` has. Two tricks that helped me:

- Use `data()` to pull up all the datasets that `animint2` has. (Thirty-three!) Then just copy/paste the list into a Dataset section in `_pkgdown.yaml`.
- Use [`ggplot2`'s YAML file](https://github.com/tidyverse/ggplot2/blob/main/_pkgdown.yml) as a reference. `animint2` is a fork of `ggplot2`, so there's lots of similarities. Don't mindlessly copy/paste, though.

Watch out for typos. Whitespace is relevant in YAML.


## Changing Appearances

There are [lots of ways to customize](https://pkgdown.r-lib.org/articles/customise.html) the website. I edited the typefaces (fonts) in the `_pkgdown.yml` file via `template > bslib`. To make finer and subtler modifications, I created an `extra.css` file in `pkgdown/` and edited the appearance of the site that way.


## Make the Site Appear on GitHub Pages

There are two ways to make this happen:

1. Upload the `docs/` directory to the GitHub repository.
2. Get a GitHub Actions workflow going that re-generates the website every time you push a commit.

I don't know how to do the second thing, so I did the first. `pkgdown` adds `docs/` to `.gitignore` by default. Delete it or comment it out. Then push your commits.


## Maintaining the Website

This is probably the most important section for most readers.




## See Also

Some documentation that may be useful:

- [Managing GitHub Credentials](https://usethis.r-lib.org/articles/git-credentials.html)
- [`pkgdown` Documentation](https://pkgdown.r-lib.org/index.html)
- [Information on `usethis::use_pkgdown`](https://usethis.r-lib.org/reference/use_pkgdown.html)
- [GitHub-flavored Markdown Specification](https://github.github.com/gfm/)