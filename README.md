The repository is linked to the publication mentioned below.

## Article
### "The Mean Shape under the Relative Curvature Condition"
### Taheri, Mohsen, Stephen M. Pizer, and Jörn Schulz. Submitted to the Journal of Computational and Graphical Statistics.

## The manuscript provides shape statistics for elliptical tubes, utilizing a relative curvature condition to ensure avoidance of self-intersections.

## The Paper
[ETRep_arxiv.pdf](ETRep_arxiv.pdf)

## Cite
```
@misc{taheri2024mean,
      title={The Mean Shape under the Relative Curvature Condition}, 
      author={Mohsen Taheri and Stephen M. Pizer and Jörn Schulz},
      year={2024},
      eprint={2404.01043},
      archivePrefix={arXiv},
      url={https://doi.org/10.48550/arXiv.2404.01043},
      primaryClass={stat.ME}
}
```


## Data and R scripts
---

Please read the **Code_Description.pdf**
[Code Description](Code_Description.pdf)

---

The primary script file is `main.R`, supported by two additional scripts in the `Functions` folder: `ETRep_Functions.R`, which contains functions specific to ETRep analysis, and `ETRep_MathFunctions.R`, which includes general mathematical functions. An ETRep captures the characteristics of *elliptical tubes* (e-tubes), including the size and orientation of its elliptical cross-sections, positioned according to the material frames along the spine.

---

The `main.R` script is divided into two main sections: **1. Transformation** and **2. Simulation**.

- **Transformation**: This section provides examples for calculating both intrinsic and non-intrinsic means, using intrinsic and non-intrinsic transformations between two e-tubes represented by their corresponding *elliptical tube representations* (ETReps), as discussed in the main manuscript.

- **Simulation**: This section provides an example of ETRep simulation, as discussed in the article's Supplementary Materials. 

---

## Images from the article
![Fig1](Fig1.jpg)
![Fig2](Fig2.jpg)
![Fig3](Fig3.jpg)


### A depiction of the simulated ETReps and their mean shape.
<img src="https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/d5122c87-bead-4ebe-9c75-58d03f72b1de" width="700">

### Visualization of a valid transformation without any self-intersections
![Fig5](Fig5.jpg)




