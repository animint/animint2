# Welcome to the Animint2 Designer Manual web site!
This manual explains how to design and create interactive data visualizations using the R package animint2. This package is an updated version of [animint](http://members.cbio.mines-paristech.fr/~thocking/animint-book/Ch00-preface.html)

## What's animint2

Chapter 1 | Preface
        
          | What's new? Comparison with animint
          
Chapter 2 | animint2
          | Inside animint2. Dependencies
          | Structure of the pacakge
          | Possibilities
            
Chapter 3 | ggplot2 and animint2

## animint2 and the grammar of graphics

Chapter 4 | What's the grammar of graphics? Overview of data analysis and visualization.

Chapter 5 | animint2, the basics. How to create an interactive graphic from ggplot2 in seconds.

Chapter 6 | Advanced options
          | showSelected
          | clickSelects
          
Chapter 7 | Customizing animint2
          | hyperlinks, tooltips, data-driven selector variable names

Chapter 8 | Publishing your interactive data visualizations on the web.

## Examples & Practice

Chapter 9 | Examples based in ggplot2

Chapter 10 | How to implement real examples from the web, media in animint2 

## Future implementations

Chapter 11 | Open Source
           | How you can contribute
          
Chapter 12 | Future implementation



## [Chapter 1: Preface](#preface)

#### History and purpose of the grammar of graphics
Most computer systems for data analysis provide functions for creating plots to visualize patterns in data. The oldest systems provide very general functions for drawing basic plot components such as lines and points (e.g. the graphics and grid packages in R). If you use one of these general systems, then it is your job to put the components together to form a meaningful, interpretable plot. The advantage of general systems is that they impose few limitations on what kinds of plots can be created. The disadvantage is that general systems typically do not provide functions for automating common plotting tasks (axes, panels, legends).

To overcome the disadvantages of these general plotting systems, charting packages such as lattice were developed (Sarkar, 2008). Such packages have several pre-defined chart types, and provide a dedicated function for creating each chart type. For example, lattice provides the bwplot function for making box and whisker plots. The advantage of such systems is that they make it much easier to create entire plots, including a legend and panels. The disadvantage is the set of pre-defined chart types, which means that it is not easy to create more complex graphics.

**What's the grammar of graphics?**
Newer plotting systems based on the grammar of graphics are situated between these two extremes. Wilkinson proposed the grammar of graphics in order to describe and create a large class of plots (Wilkinson, 2005). Wickham later implemented several ideas from the grammar of graphics in the ggplot2 R package (Wickham, 2009). The ggplot2 package has several advantages with respect to previous plotting systems.

Like general plotting systems, and unlike lattice, ggplot2 imposes few limitations on the types of plots that can be created (there are no pre-defined chart types).
Unlike general plotting systems, and like lattice, ggplot2 makes it easy to include common plot elements such as axes, panels, and legends.

All of the previously discussed plotting systems are intended for creating static graphics, which can be viewed equally well on a computer screen or on paper. However, the main topic of this manual is animint, an R package for interactive graphics. In contrast to static graphics, interactive graphics are best viewed on a computer with a mouse and keyboard that can be used to interact with the plot.

Since many concepts from static graphics are also useful in interactive graphics, the animint package was implemented as an extension of ggplot2. 

animint2 was released in 2017 to solve... 

#### What's new in animint2? 
animint2 is a redesign of animint with a cleaner syntax.
Comparison with animint

#### Why you'll find animint2 useful?

animint2 is a great library to create interactive graphics and to get your ggplot2 graphics ready for the web. The magic of animint2 is that with only a few lines of code you'll be able to transform your static ggplot2 graphics to interactive ones. It allows you to create a new bunch of perspectives of the data you are visualising. 

## [Chapter 2: animint2](#animint2)

animint2 is a redesign of animint with a cleaner syntax. 

## [Chapter 3: ](#)

## [Chapter 4: ](#)

#### What's ggplot2?

Created in 2005 by Hadley Wickham, ggplot2 is a libary based on Leland Wilkinson's Grammar of Graphics, a general scheme for data visualization which breaks up graphs into semantic components such as scales and layers. You provide the data, tell ggplot2 how to map variables to aesthetics, what graphical primitives to use, and it takes care of the details.

We are going to cover the basic steps to run and work with ggplot2. For a more detailed explanation you can read its [own designer guide](https://ggplot2.tidyverse.org/)

**How to install ggplot2**

```
install.packages("ggplot2")
```

## [Chapter 5: animint2, the basics](#)

Once installed, let's create our first graphic. We are going to start with one of the most basic: a histogram. Regarding the data, we are going to use diamonds as our source.

```
ggplot(diamonds, aes(carat)) +
  geom_histogram()
```

#### animint2 installation

To install animint2 we have to run this code in R:
(Review this after submission to CRAN)

```
devtools::install_github("faizan-khan-iit/ggplot2@validate-params")
devtools::install_github("tdhock/animint2")
```


## [Chapter 6: ](#)

## [Chapter 7: ](#)

## [Chapter 8: ](#)

animint2 is designed to create interactive graphics. Once you have them, you have a bunch of options to publish them. The ideal output would be the web but in this chapter all options are covered. As a user, you can publish your animint2 graphics:

* on your personal computer.
* in R Markdown documents.
* on gist.github.com and view them on bl.ocks.org.
* using any web server, including GitHub pages.

#### personal computer

#### R Markdown

Confirm that animint2 will allow to publish via R Markdown

#### bl.ocks.org

To publish R visualizations in bl.ocks first we have to deal with [GitHub Gist](https://help.github.com/articles/about-gists/). Gist was originally meant for sharing code, but it also works for sharing animints.

To publish an animint on Gist, you need: 
* 1) a GitHub account (Sign up [here](https://github.com/join) for free)
* 2) the gistr package. 

If you have already set up your Githu account, now you can install the gistr package. Write in R:

```
if(!require(gistr))install.packages("gistr")
````

(Rewrite paragraph)
Now you should be able to use animint2gist(viz) to publish viz as a new gist. That command will post the files to a new gist, for example https://gist.github.com/tdhock/2b029fe9abc8eb300f9f/. It will then open a new web browser window at the corresponding bl.ocks.org URL, for example http://bl.ocks.org/tdhock/raw/2b029fe9abc8eb300f9f/

NOTE: Should we include an example in ObservableHQ?
https://beta.observablehq.com/

## [Chapter 9: ](#)

## [Chapter 10: Real examples](#)

Proposed examples to replicate in gglpot2 and make them interactive with animint2.

Scatterplot from the NYT:
https://www.nytimes.com/interactive/2018/06/13/upshot/boys-girls-math-reading-tests.html

Interactive histogram from WaPo:
https://www.washingtonpost.com/graphics/2018/politics/house-primaries-candidates/?noredirect=on&utm_term=.a375ae92ecc2

## [Chapter 11: Open source](#)

* Open source

[Open-source software](https://en.wikipedia.org/wiki/Open-source_software) is a type of computer software, developed in a collaborative public manner, whose source code is released under a license in which the copyright holder grants users the rights to study, change, and distribute the software to anyone and for any purpose.

* Test-driven development

## [Chapter 12: ](#)
