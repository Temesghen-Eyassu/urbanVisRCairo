# urbanVisRCairo
 An R package for analyzing and visualizing structured and unstructured urban building patterns in Cairo using spatial vector data. It was developed using R version 4.4.2.
## Description 
urbanVisRCairo is an R package designed to provide comprehensive tools for analyzing and visualizing urban building footprints in Cairo. It supports both structured and unstructured building patterns by leveraging spatial vector data and advanced spatial metrics.
The package offers functionalities for calculating spatial attributes such as building area, compactness, and other morphological indicators. It also implements hexagonal binning techniques for spatial aggregation and thematic mapping, enabling the classification and visualization of built environments based on area bins.
This package is ideal for urban morphology research, spatial planning, and visualization of urban form dynamics in Cairo, facilitating better understanding of urban structure and development patterns.

## Installation

To install the **urbanVisRCairo** package, use the following steps:

1. **Install remotes package if not installed** 
    ```r
    install.packages("remotes")
    ```
2. **Install the Cairo_Structured-Unstructured_Buildings package from GitHub**:

    ```r
    library(remotes)
    remotes::install_github("Temesghen-Eyassu/urbanVisRCairo")
    ```
3. **Load the package**:

    ```r
    library(urbanVisRCairo)
    ```

---

### **Geometric Visualization**

#### Purpose 
To provide a quick visual comparison of the spatial distribution, shape, and layout characteristics of buildings in the structured and unstructured datasets. This helps identify differences in urban form, such as block organization, building alignment, and clustering patterns, at a glance. 

        -Left (p1): Unstructured buildings (raw or irregular footprints).
        -Right (p2): Structured buildings (planned or regular footprints
![Image](https://github.com/user-attachments/assets/2cfac23d-4f31-4620-a107-9586c89fd2c5)

### **Compactness and Area Maps with Histogram**
#### Purpose 
This section demonstrates how to visualize spatial polygon data by mapping two key geometric attributes: compactness and area. For each attribute, the script produces a combined plot featuring: 

       - A map with polygons colored by the selected attribute, providing spatial context.
       - A histogram illustrating the distribution of the attribute values across all polygons.
This combined visualization allows users to explore spatial patterns and attribute distributions simultaneously, making it easier to analyze the shape and size characteristics of geographic features such as buildings or land parcels. This approach helps not only users who are new to spatial data analysis but also those encountering this specific dataset for the first time to understand how the attribute values are distributed both spatially and statistically across the dataset. Furthermore, the combined maps and histograms are arranged in the following order to facilitate comparison between structured and unstructured buildings:

      1. Structured Buildings: Compactness map accompanied by its histogram
      2. Structured Buildings: Area distribution map accompanied by its histogram
      3. Unstructured Buildings: Compactness map accompanied by its histogram
      4. Unstructured Buildings: Area distribution map accompanied by its histogram
    
![Image](https://github.com/user-attachments/assets/f5cdac2a-f168-49d4-8fe2-d4c433f4d255)

**How to Visualize the Maps and Graphs**

To generate and view the combined maps and histograms for compactness and area, simply run the following code after sourcing or running the 3_compactness_area_graphs script:

    final_plot <- analyze_building(
          unstructured_path = "path/to/unstructured_buildings.gpkg",
          structured_path = "path/to/structured_buildings.gpkg"
       )
     print(final_plot)

***Note**: You must load the packages sf, dplyr, ggplot2, viridis, and patchwork before running final_plot
### **Compactness Analysis and Visualization for Structured vs. Unstructured Buildings**
#### Purpose 
This tool helps users quickly explore and compare the shape characteristics of building footprints in different urban contexts, making it easier to interpret spatial patterns of building compactness both visually (maps) and statistically (histograms/density).

This function, plot_compactness_area_graphs(), processes two spatial polygon datasets representing structured and unstructured buildings. It calculates a compactness metric — a geometric shape measure defined as 
4𝜋×area/perimeterSquare— which quantifies how close each building’s shape is to a perfect circle (values closer to 1 indicate more compact, regular shapes).

What it does:

  -Reads spatial datasets of building footprints from GeoPackage files.

  -Computes compactness values for each building polygon.

  -Creates thematic maps of compactness for both structured and unstructured buildings using color gradients.

  -Generates a combined histogram and density plot to compare the distribution of compactness values across the two building types.

  -Combines these visualizations into a cohesive layout using patchwork, allowing for easy side-by-side spatial and statistical comparison.

![Image](https://github.com/user-attachments/assets/66a31c0b-1b48-4d68-9ad5-27778044d99b)

The density scale on the y-axis represents the probability density — that is, the relative likelihood of compactness values falling within each bin, normalized so the total area under the histogram sums to 1.

**How to Visualize Compactness Analysis

To generate the compactness comparison plots for structured and unstructured buildings, you can use the function plot_compactness_area_graphs() from the script 4_compactness_analysis.R. 
Load and run the function, passing the file paths of your datasets:

     plot_compactness_area_graphs(
          unstructured_path = "path/to/unstructured_buildings.gpkg",
          structured_path = "path/to/structured_buildings.gpkg"
     )



### **Spatial hexbin aggregation and comparative visualization of building areas in structured vs. unstructured**
#### Purpose 
To compare and visualize the spatial distribution and average area of buildings between structured and unstructured datasets using hexagonal binning.

What it does:

Reads two building datasets, calculates building areas, creates a shared hexagonal grid over their combined extent, filters to a core study area, aggregates building counts and average area per hex, removes outliers, and produces side-by-side hexbin maps and density plots for both datasets.

![Image](https://github.com/user-attachments/assets/c6462f3d-79fc-49fd-a46b-8e64fe459106)

The **Avg Area** represents the average surface area of building footprints within each hexagonal cell. It quantifies the typical size of buildings in square meters (or map units) by averaging the total ground area covered by all buildings in that cell. This helps compare building sizes spatially between structured and unstructured areas.

The **density** on the y-axis represents the relative frequency of hexagonal cells with a given average building area on the x-axis. It shows how building sizes are distributed spatially, with higher values indicating more hex cells having that average area. The values (e.g., 0 to 0.0075) reflect the probability density, not counts, so they sum to 1 over the range.

**How to Visuaize 

After sourcing or running 5_building_area_hexbins.R, simply run the following function:

     plot_hexbin_area_analysis(
       unstructured_path = "path/to/unstructured_buildings.gpkg",
       structured_path = "path/to/unstructured_buildings.gpkg"
     )


### **Statistical Visualization of Building Compactness: Boxplot, Violin, and Density Analysis**
#### Purpose 
This function aims to support urban morphological analysis by calculating a compactness index (4πA/P²) for buildings and generating statistical visualizations that compare how compact or sprawling the buildings are across structured (planned) and unstructured (unplanned) areas. The resulting plots (boxplot, density, and violin) highlight key distributional characteristics such as variability, central tendency, and potential outliers.

What it does:

•	Reads two building datasets (structured and unstructured areas) in GeoPackage format.

•	Calculates compactness for each building using the formula:

         			 Compactness=4π×Area/PerimeterSquare  
             
•	Labels buildings as "Structured" or "Unstructured".

•	Combine the two datasets into one for comparison.

•	Generates three plots to visualize the compactness distribution:

    Boxplot — shows media, spread, and outliers.
    
    Violin plot — shows distribution shape and density.
    
    Density plot — compares the probability distribution of compactness between the two types.
    
•	Performs a Wilcoxon statistical test to compare differences in compactness between the two groups.

•	Returns a single patchwork plot combining all three visualizations.

![Image](https://github.com/user-attachments/assets/fb3f4201-9945-43e2-938d-28aeb5f33ac1)


The p-value from the statistical test (Wilcoxon test here) comparing the compactness between structured and unstructured buildings, whereas u refers to the mean compactness (μ), and the number after ± is the standard deviation (SD).

**How to Visuaize 

After sourcing or running 6_Boxplot_violin.R, simply run the following function:

     p<-compare_building_compactness(
       unstructured_path = "path/to/unstructured_buildings.gpkg",
       structured_path = "path/to/unstructured_buildings.gpkg"
     )
     print(p)


