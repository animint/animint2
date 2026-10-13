acontext("aes(href)")

color.df <- data.frame(
  x=c(1, 1, 2, 1, 2),
  university=c(
    "Stanford",
    rep("UC Berkeley", 2),
    rep("Oregon State", 2)),
  color=c("red", "blue", "gold", "orange", "black"))
university.df <- as.data.frame(table(color.df$university))
names(university.df) <- c("university", "colors")

test_that("clickSelects and href is an error", {
  viz <- list(
    colors=ggplot()+
      geom_point(aes(
        x, university, color=color, href=color),
        clickSelects="university",
        data=color.df)+
      scale_color_identity())
  expect_error({
    animint2dir(viz, open.browser=FALSE)
  }, "clickSelects can not be used with aes(href)", fixed=TRUE)
})

test_that("aes(href) becomes <a href>", {
  viz <- list(
    universities=ggplot()+
      geom_bar(aes(
        university, colors,
        id=university),
        clickSelects="university",
        data=university.df, stat="identity"),
    colors=ggplot()+
      geom_point(aes(
        x, university, color=color,
        href=paste0("http://en.wikipedia.org/wiki/", color)),
        showSelected="university",
        data=color.df, size=5)+
      scale_color_identity(),
    first=list(university="UC Berkeley"))
  info <- animint2HTML(viz)
  expected.links <- c(
    "http://en.wikipedia.org/wiki/blue",
    "http://en.wikipedia.org/wiki/gold")
  expect_links(info$html, expected.links)
})

test_that("clicking updates href", {
  stanford.html <- clickHTML("id"="Stanford")
  expect_links(stanford.html, "http://en.wikipedia.org/wiki/red")
})

test_that("clicking updates href (again)", {
  osu.html <- clickHTML("id"="Oregon State")
  expect_links(osu.html,
               c("http://en.wikipedia.org/wiki/orange",
                 "http://en.wikipedia.org/wiki/black"))
})

test_that("aes(href) works with geom_polygon", {
  USpolygons <- map_data("state")
  USpolygons$href <- paste0("https://en.wikipedia.org/wiki/", USpolygons$region)
  viz.href <- list(
    map=ggplot()+
      ggtitle("click a state to read its Wikipedia page")+
      coord_equal()+
      geom_polygon(
        aes(x=long, y=lat, group=group, href=href),
        data=USpolygons, fill="black", colour="grey")
  )
  info <- animint2HTML(viz.href)
  (expected.vec <- unique(USpolygons$href))
  expect_links(info$html, expected.vec)
})

test_that("aes(href) works with geom_text", {
  text.df <- data.frame(commit=c(
    "11ab05ab1b6cf5ca602f8632bc2a72d47e85583a", "38328a3a69f5fead3abac8560a6c9f42f358a3f7", 
    "4aadde8f5a51cd7c8f3889964e7280432ec65bbc", "4bda6dac7ede429434412ede26b71cfada76f33d", 
    "793f8545c363d222de18ac892bc7abb80154e724", "b417000b570133d1d014a4745163142d0c68e195", 
    "0216983c51e03e3f61d5e6f08f4ba0c42cceb22c", "0c7c90cbe0f5d742b9703e3f07dd4c7c52e08523", 
    "0ebe04b8afdb2d159654e5bdeedf381e17b67141", "20b463c9592c07d05ad2e89a25f84f0b998d6154", 
    "82718b612973ff25fda8e6269c817d359985a85e", "851467f49e0c3b1a1d3fb7e9273dac706f0a7453", 
    "906d026befa1b8c63472f1822714d40b91114103", "b3acef8f9694cd4fd45f6fbf15f41943d9efced5", 
    "b3acf2ad13848150e87c414f3edcc413c71fd977", "bbe7563f9e8d9fb172eeade319850db0f5c6fe20", 
    "e13f99379f09dd928fd186950828903d97e7a844", "1cb18e384728c46eb620edbadf930d677279e304", 
    "3a5ed9b4eda89b47c7bbcfe96930947d067d199e", "460b919ed97f2b04079c69a606fe76dd2cd07e1f", 
    "588e0725320eacc5d8fc296ee9da4967cee198af", "6f360be0b2a6cf425f6df751ca9a99ec5d35ed93", 
    "862d8ac897489e92d2d2db1ad3bc59498098280f", "ba32f3cba38ec270587e395f6e6c26a80be36be6", 
    "bde23bcdcbcf0b41d4f7cc097e0fe889c13a431f", "0310dc18b200904d2d8c084caecd9ea885afce0c", 
    "0f942123fc2221f3166caca8c61ce6a2e7ecf36e", "19065fba64f9b48930d8e51573d01976cbfb25a6", 
    "1bc1b4095b99ce101b87b38017b6854614934380", "3fa8b20435d33b3d4b5c26fd9b0ac14c10b98800", 
    "53e15854bb2c4d2faa7184a3c782cce40e9210f2", "72bcdea8df042a4c6ddf65ea1394e8788fab4810", 
    "85adf09e3463838d547977ae9bc75e3b37f9cbaf", "fbbf8c43cadd32c8db4ae8035029c251f37c18d0", 
    "0216983c51e03e3f61d5e6f08f4ba0c42cceb22c", "14d7f8b2f2fa050df6ebb0b059840659348777d1", 
    "20b463c9592c07d05ad2e89a25f84f0b998d6154", "4d30c7f2437dd17debd19d34ed1c28f6bf8e5484", 
    "6a70af68416e8310f2193b4a644a57e6c8253592", "77ba20e4eaf8edb90421207b76d4c483bd77e52f", 
    "8ff68f0c25961c95aa077ca16e06ad49b555ec07", "a8ad00fc93f1f5d293ee48b63f39468febef6f75", 
    "cabd30bf21694c4e004a9010073836007436beb3", "cfa947febfa69eaee828c17d85999639df446294", 
    "d3d00192e025cc7c967ed14984169eeb77410ff7"))
  viz <- animint(
    ggplot()+
      geom_text(aes(
        0, seq_along(commit), label=commit),
        clickSelects="commit",
        data=text.df),
    ggplot()+
      geom_text(aes(
        0, seq_along(commit), label=commit,
        href=paste0("https://github.com/Rdatatable/data.table/commit/", commit)),
        showSelected="commit",
        data=text.df))
  viz
})
