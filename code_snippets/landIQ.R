
# so I have the 2021 LandIQ crop map as a shapefile,
# downloaded from https://www.landiq.com/land-use-mapping
# and unzipped by hand.

# Let's explore.

# TODO: Same dataset is also available as a geodatabase -- would that be easier / more useful to work from?

library(sf)
library(tidyverse)
library(scales)

crops <- read_sf(("external_data/i15_Crop_Mapping_2021_SHP/i15_Crop_Mapping_2021.shp")) |>
	# st_make_valid() |> # will need this for mapping, but slow and currently just using acres
	# urban lands are inconsistently labeled in other columns,
	# and are not of interest here -- dropping so we don't have to special-case them later
	filter(!SYMB_CLASS %in% c("U", "UL"))


# par(mar = c(0, 0, 0, 0))
# plot(st_geometry(crops), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

# ggplot(crops[crops$CLASS1 != "**",]) + aes(color=CLASS1) + geom_sf()+theme_void()


cropnames <- read.delim("code_snippets/landiq_crop_mapping_codes.tsv")
cropnames$subcl_str <- paste0(
	cropnames$CLASS,
	cropnames$SUBCLASS |> as.character() |> replace_na(""))

# TODO: See lines 908 - end (1259) of i15_Crop_Mapping_2021.shp.xml to identify
#	useful info that could be extracted from other columns.
# Likely targets include
# 	- YR_PLANTED
#	- peak NDVI dates (ADOY*) (!Can be negative if this crop peaked in 2020!)
#	- SEN_CROP, EMRG_CROP: for multiyear crops that peaked before or after 2021
#	- percent of field cropped (PCNT*)
#	- special conditions (SPECOND*):
#		Documented to include potentially useful states like "cover crop",
#		"abandoned", "tilled land".
#		Quick inspection shows 2021 data use it almost exclusively for young
#		orchards (< 3 years old)
#	- Irrigation status (IRR_TYP1PA) and infrastructure type (IRR_TYP1PB)
#		In 2021 data status seems barely used and type is never used --
#		maybe these will be added to in future years?

crops_named <- crops |>
	left_join(cropnames, by = c("MAIN_CROP" = "subcl_str"))


# random map plotting example: grain and citrus in SB Co
# crops_named |>
# 	filter(COUNTY == "Santa Barbara", CLASS2 %in% c("G","C")) |>
# 	ggplot() +
# 	aes(color=subclass_name) +
# 	geom_sf() +
# 	theme_bw()


# Total acreage by class and subclass
# TODO need to handle multicropping here! For now only counting main crop
names_acres <- crops_named |>
	as_tibble() |> # spatial aggregation not needed here and sloooow
	select(SYMB_CLASS, CLASS, SUBCLASS, class_name, subclass_name, MAIN_CROP, ACRES)

acres_by_subclass <- names_acres |>
	group_by(CLASS, class_name, SUBCLASS, subclass_name) |>
	summarize(acres = sum(ACRES))

acres_by_class <- acres_by_subclass |>
	group_by(CLASS, class_name) |>
	summarize(acres = sum(acres))

plt <- acres_by_subclass |>
	ggplot() +
	aes(acres, reorder(subclass_name, acres), fill = class_name) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(50)) +
	theme(
		legend.position = "inside",
		legend.position.inside = c(0.8, 0.3),
		axis.text.y = element_text(size = 8, lineheight = 0.5)) +
	ggtitle('CA acres by "main crop", LandIQ 2021')
ggsave("acres_by_subclass.png", plt, width = 8, height = 12)

plt <- acres_by_class |>
	ggplot() +
	aes(acres, reorder(class_name, acres)) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(20)) +
	ggtitle('CA acres by class of "main" crop, LandIQ 2021')
ggsave("acres_by_class.png", plt, width = 8, height = 12)

# How much do we need to worry about non-main crops?
crops_named |>
	as_tibble() |>
	group_by(MULTIUSE) |>
	summarize(n = n()) |>
	arrange(n) |>
	mutate(pct = n/sum(n))
# # A tibble: 5 × 3
#   MULTIUSE      n        pct
#   <chr>     <int>      <dbl>
# 1 M             1 0.00000235
# 2 Q           422 0.000992  
# 3 T          3622 0.00851   
# 4 D         22629 0.0532    
# 5 S        398860 0.937     

# --> almost 94% of fields are single-cropped, most of the rest double
# (I did it by acres too [not shown] and the numbers are very similar)

acres_all_cycles <- crops_named |>
	as_tibble() |>
	select("SYMB_CLASS", "MULTIUSE", contains("CROPTYP"), "MAIN_CROP", "ACRES") |>
	pivot_longer(
		contains("CROPTYP"),
		names_prefix = "CROPTYP",
		names_to = "cycle",
		values_to = "croptype") |>
	filter(croptype != "****") |>
	left_join(cropnames, by = c("croptype" = "subcl_str"))

acres_by_subclass_all_cycles <- acres_all_cycles |>
	group_by(cycle, CLASS, class_name, SUBCLASS, subclass_name) |>
	summarize(acres = sum(ACRES)) |>
	# Used for ordering plot bars below
	group_by(subclass_name) |>
	mutate(tot_acres_in_subclass = sum(acres))

acres_by_class_all_cycles <- acres_by_subclass_all_cycles |>
	group_by(cycle, CLASS, class_name) |>
	summarize(acres = sum(acres)) |>
	# Used for ordering plot bars below
	group_by(class_name) |>
	mutate(tot_acres_in_class = sum(acres))

plt <- acres_by_subclass_all_cycles |>
	ggplot() +
	aes(acres, reorder(subclass_name, tot_acres_in_subclass), fill = cycle) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(50)) +
	theme(
		legend.position = "inside",
		legend.position.inside = c(0.8, 0.3),
		axis.text.y = element_text(size = 8, lineheight = 0.5)) +
	ggtitle('CA acres by crop and cycle, LandIQ 2021')
ggsave("acres_by_subclass_and_cycle.png", plt, width = 8, height = 12)

plt <- acres_by_class_all_cycles |>
	ggplot() +
	aes(acres, reorder(class_name, tot_acres_in_class), fill = cycle) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(20)) +
	theme(
		legend.position = "inside",
		legend.position.inside = c(0.8, 0.3)) +
	ggtitle('CA acres by crop class and cycle, LandIQ 2021')
ggsave("acres_by_class_and_cycle.png", plt, width = 8, height = 12)



plt <- acres_by_subclass_all_cycles |>
	ggplot() +
	aes(acres, reorder(subclass_name, tot_acres_in_subclass), fill = class_name) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(50)) +
	theme(
		legend.position = "inside",
		legend.position.inside = c(0.8, 0.3),
		axis.text.y = element_text(size = 8, lineheight = 0.5)) +
	ggtitle('CA acres by crop, all crop cycles, LandIQ 2021')
ggsave("acres_by_subclass_all_cycles.png", plt, width = 8, height = 12)

plt <- acres_by_class_all_cycles |>
	ggplot() +
	aes(acres, reorder(class_name, tot_acres_in_class)) +
	geom_col() +
	theme_bw() +
	ylab(NULL) +
	scale_y_discrete(labels = wrap_format(20)) +
	ggtitle('CA acres by crop class, all crop cycles, LandIQ 2021')
ggsave("acres_by_class_all_cycles.png", plt, width = 8, height = 12)
