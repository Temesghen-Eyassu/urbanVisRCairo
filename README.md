# urbanVisRCairo
 An R package for analyzing and visualizing structured and unstructured urban building patterns in Cairo using spatial vector data. It was developed using R version 4.4.2.
## Description 
urbanVisRCairo is an R package designed to provide comprehensive tools for analyzing and visualizing urban building footprints in Cairo. It supports both structured and unstructured building patterns by leveraging spatial vector data and advanced spatial metrics.
The package offers functionalities for calculating spatial attributes such as building area, compactness, and other morphological indicators. It also implements hexagonal binning techniques for spatial aggregation and thematic mapping, enabling the classification and visualization of built environments based on area bins.
This package is ideal for urban morphology research, spatial planning, and visualization of urban form dynamics in Cairo, facilitating better understanding of urban structure and development patterns.

## Installation

To install the **urbanVisRCairo** package, use the following steps:

1. **Install the devtools package** (if not already installed)
    ```r
    install.packages("devtools")
    ```
2. **Install the Cairo_Structured-Unstructured_Buildings package from GitHub**:

    ```r
    library(devtools)
    install_github("Temesghen-Eyassu/urbanVisRCairo")
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
    
![Image](https://github.com/user-attachments/assets/dc7be936-5770-40f8-beec-7de2c8f184fb)

**How to Visualize the Maps and Graphs**

To generate and view the combined maps and histograms for compactness and area, simply run the following code after sourcing or running the 3_compactness_area_graphs script:

        structured_compactness_plot <- plot_map_and_histogram(buildings_structured, "compactness", "Structured")
        structured_area_plot <- plot_map_and_histogram(buildings_structured, "area", "Structured", fill_option = "D")
        unstructured_compactness_plot <- plot_map_and_histogram(buildings_unstructured, "compactness", "Unstructured")
        unstructured_area_plot <- plot_map_and_histogram(buildings_unstructured, "area", "Unstructured", fill_option = "D")
        
        final_plot <- (structured_compactness_plot / structured_area_plot /
               unstructured_compactness_plot / unstructured_area_plot)
               print(final_plot)


