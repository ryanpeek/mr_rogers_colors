# 2021 R. Peek

# Using sweater color image from here:
# http://blog.neighborhoodarchive.com/2018/03/make-your-own-sweater-color-print.html
# https://www.theawl.com/2017/05/every-color-of-cardigan-mister-rogers-wore-from-1979-2001/

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(magick)
library(scales)
library(glue)
library(imager)


# Get Colors From Image ---------------------------------------------------

get_palette <- function(img, n=8, cs="RGB"){

  # create data from image
  dat <-img %>% image_resize("500") %>%
    image_quantize(max=n, colorspace=cs) %>%
    magick2cimg() %>%  # convert to cimg/list
    RGBtoHSV() %>% # add hue to list
    as.data.frame(wide="c") %>%  # make data wide by color
    # now format variables
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           value = c.3) %>%
    count(hex, hue, sat,value, sort=TRUE) %>%
    mutate(colorspace = cs)
  # return output as data.frame
  return(dat %>% dplyr::select(colorspace,hex,hue,sat,value,n))
}


# APPLY FUNCTION -------------------------------------------------

mr_jpg <- image_read("images/mr_rogers_sweatercolors.jpeg")
mr_png <- image_read("images/mr_rogers_sweatercolors_1979-2001.png")

# apply function
colsnumb <- 24
get_palette(mr_jpg, n = colsnumb, cs="RGB") %>%
  pull(hex) # get only hex values

# MAP FUNCTION FOR ALL COLORSPACE TYPES -----------------------------------

# see colorspace_types() and drop gray and Luma scales:
colorspace_types_filt <- colorspace_types()[!grepl("Luma|Gray", colorspace_types())]

## for JPG

# set parameters to map over
params_jpg <- list(im=list(mr_jpg),
                   n=colsnumb, # number of colors
                   cs=colorspace_types_filt)
# JPG: map over list
my_colors_jpg <- pmap_df(params_jpg, get_palette) %>%
  # add number of colors by colorspace
  group_by(colorspace) %>% add_tally(name = "ncolors") %>%
  ungroup()

## for PNG

# set parameters to map over
params_png <- list(im=list(mr_png),
                   n=colsnumb,
                   cs=colorspace_types_filt)

# PNG: map over list
my_colors_png <- pmap_df(params_png,get_palette) %>%
  group_by(colorspace) %>% add_tally(name = "ncolors") %>%
  ungroup()


# Polar Plots --------------------------------------------------------------

# plot
(polar_jpg <- my_colors_jpg %>%
   group_by(colorspace) %>%
   mutate(ypos=row_number(hue)) %>%  ## alter stacking order
   ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
   geom_tile() +
   scale_fill_identity() +
   scale_y_continuous(breaks=NULL) +
   theme_void() +
   coord_polar() +
   expand_limits(y=-1) +
   labs(title="Mr. Rogers Sweater Colors",
        subtitle="JPG"))

(polar_png <-my_colors_png %>%
    group_by(colorspace) %>%
    mutate(ypos=row_number(hue)) %>%  ## alter stacking order
    ggplot(aes(x=colorspace, y=ypos, fill=hex)) +
    geom_tile() +
    scale_fill_identity() +
    scale_y_continuous(breaks=NULL) +
    theme_void() +
    coord_polar() +
    expand_limits(y=-1) +
    labs(title="Mr. Rogers Sweater Colors (1979-2001)",
         subtitle=glue("Palette with max of {colsnumb} colors"),caption="{data from: http://blog.neighborhoodarchive.com/2018/03/make-your-own-sweater-color-print.html}"))

# plot side by side?
library(patchwork)
polar_jpg + polar_png + plot_layout(ncol=2)


# Color Grid Plot --------------------------------------------------------

# make a grid of colors for given image
# this is really best for palettes colors N =< 12
# set parameters to map over
params_grid_png <- list(im=list(mr_png),
                   n=12,
                   cs=colorspace_types_filt)

# PNG: map over list
my_grid_png <- pmap_df(params_grid_png,get_palette) %>%
  group_by(colorspace) %>% add_tally(name = "ncolors") %>%
  ungroup()



# group by colorspace
#my_colors_png %>%
(my_grid_png %>%
  group_by(colorspace) %>%
  mutate(ypos=row_number(value)) %>% # order based on color value
  ggplot(aes(x=fct_infreq(colorspace), y=ypos, fill=hex)) +
  geom_tile() +
  geom_text(aes(label=hex), color="#ffffffbe",
            size=3, family="Roboto Condensed") +
  scale_fill_identity() +
  scale_y_continuous(breaks=NULL) +
  theme_void(base_family="Roboto Condensed") +
  coord_flip(ylim=c(1,12)) +
  theme(axis.text = element_text(color = "black", family="Roboto Condensed", hjust=1)) +
  labs(caption="Colorspaces with n=12") -> grid_p2)


# Plot Together -----------------------------------------------------------

polar_png + grid_p2 + plot_layout(ncol=2, widths = c(0.9, 1))

ggsave(filename = "output/mr_rogers_sweater_color_palette.jpg", width = 11, height = 8, dpi=300, scale = 1.1)

