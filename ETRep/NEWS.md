# ETRep 1.2.2

## Updates
- Debugging NEWS file

---

# ETRep 1.2.1

## Updates
- The package description has been updated to include the published version of  
  the paper *"The Mean Shape under the Relative Curvature Condition"*  
  in the *Journal of Computational and Graphical Statistics*  
  (DOI: [10.1080/10618600.2025.2535600](https://doi.org/10.1080/10618600.2025.2535600)).

- The plotting issue reported by macOS users due to the lack of native OpenGL  
  support has been fixed by introducing the helper functions  
  `etrep_open3d()` and `etrep_show3d()`.  
  These functions allow plotting to fall back to a WebGL viewer  
  and open the scene in a browser when a native `rgl` device is not available.

---

# ETRep 1.2.0

## Updates
- Added the function `tube_Surface_Mesh()`:
  - Produces a quadrilateral or triangular surface mesh of a tube as an `rgl` `mesh3d` object.

---

# ETRep 1.1.0

## Updates
- Improved clarity and precision in the package description and function documentation.
- Added the function `elliptical_Tube_Euclideanization()`:
  - Enables the Euclideanization of elliptical tubes for downstream analysis and computations.

---

# ETRep 0.1.0

## First CRAN Release
- Initial release of **ETRep**.  
- Date of publication: 2024-11-04
