\name{geom_jitterbump}
\alias{geom_jitterbump}
\alias{StatJitterbump}
\title{
Bump plot with jitter
}
\description{
A kind of parallel coordinate plot
}
\usage{
geom_jitterbump(mapping = NULL, data = NULL, geom = "line",
                position = "identity",
                na.rm = FALSE, show.legend = NA,
                smooth = 8, direction = "x", n_points = 25,
                inherit.aes = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mapping}{
Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
}
  \item{data}{
The data to be displayed in this layer.
}
  \item{geom}{
change geom
}
  \item{position}{
Position adjustment, either as a string, or the result of a call to a position adjustment function
}
  \item{show.legend}{

logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
}
  \item{smooth}{
how much smooth should the curve have? More means steeper curve.
}
  \item{direction}{
the character x or y depending of smoothing direction
}
  \item{n_points}{
how much smooth should the curve have? More means steeper curve.
}
  \item{inherit.aes}{
If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().}
}
  \item{\dots}{

}
}
\examples{
library(ggjitterbump)
library(tidyr)
library(dplyr)

dfiris <- mutate(iris, id=row_number()) \%>\%
  pivot_longer(1:4, names_to = "variable")

ggplot(dfiris, aes(x = variable, y = log(value), group = id, colour=Species)) +
  geom_jitterbump(linewidth = 0.1)+
  geom_point(alpha = 0.1)+
  guides(colour = guide_legend(override.aes = list(alpha=0.7, linewidth = 0.8)))+
  theme_bw(16)
}
