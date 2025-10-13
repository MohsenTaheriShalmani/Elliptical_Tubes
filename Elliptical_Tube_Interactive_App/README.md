## 🌐 WebApp: Elliptical Tube Interactive App (ETIA)

The **Elliptical Tube Interactive App (ETIA)** is an interactive 3D visualization and modeling platform built on top of the **[Elliptical Tube Representation (ETRep)](https://cran.r-project.org/web/packages/ETRep/index.html)** R package.

ETIA provides an intuitive and user-friendly interface for constructing, analyzing, and visualizing **elliptical tube-like structures** — 3D geometries that can represent anatomical, biological, or other elongated tubular forms.

### 🧠 Core Features

* **Elliptical Tube Generation:**
  Create and manipulate elliptical tubes using Euler angle sequences, customizable cross-sectional parameters, and connection lengths.

* **Geometric Analysis:**
  Compute volumetric and cross-sectional properties, as well as mean shapes of multiple tubes under intrinsic geometric constraints.

* **Relative Curvature Condition (RCC):**
  Integrate curvature-based alignment to ensure smooth intrinsic representations along skeletal frames.

* **Transformation Visualization:**
  Compare and visualize smooth transformations between two tubes with identical skeletal frames.

* **Interactive 3D Environment:**
  Built with `rgl` for real-time rendering, rotation, and zooming of complex 3D structures directly in the browser.

### 🧩 Architecture

ETIA is developed using:

* **R Shiny** and **Shinydashboard** for the web interface
* **ETRep** for computational geometry and skeletal modeling
* **rgl** for interactive 3D rendering
* **colourpicker** and **shinyWidgets** for customizable input controls

The app is modular and consists of three main sections:

1. **Tube Generator** – build and export elliptical tube meshes.
2. **Transformation** – define and visualize transitions between two tube geometries.
3. **About ETRep** – documentation and background on the underlying R package.

### 🚀 Live Demo

You can explore the fully functional web application here:
👉 **[Live ETIA Demo on ShinyApps.io](https://cucsym-mohsen-taheri.shinyapps.io/etrep_shinyapp/)**

### 📘 Reference

The methods and algorithms implemented in ETIA are based on the **Elliptical Tube Representation (ETRep)** framework introduced by
**Mohsen Taheri Shalmani (2025)** in:

> *Taheri Shalmani, M. (2025). "Elliptical Tube Representation: A Framework for Skeletal Shape Analysis under the Relative Curvature Condition (RCC)." Journal of Computational and Graphical Statistics.*
> [https://doi.org/10.1080/10618600.2025.2535600](https://doi.org/10.1080/10618600.2025.2535600)