The repository is linked to the publication mentioned below.

## Article
### "The Mean Shape under the Relative Curvature Condition"
### Taheri, Mohsen, Stephen M. Pizer, and Jörn Schulz. Submitted to the Journal of Computational and Graphical Statistics.

## The manuscript provides shape statistics for elliptical tubes, utilizing a relative curvature condition to ensure avoidance of self-intersections.

## The Paper
[ETRep_arxiv.pdf](https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/files/14823254/ETRep_arxiv.pdf)

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

The primary script file is `main.R`, supported by two additional scripts in the `Functions` folder: `ETRep_Functions.R`, which contains functions specific to ETRep analysis, and `ETRep_MathFunctions.R`, which includes general mathematical functions. An ETRep captures the characteristics of *elliptical tubes* (e-tubes), including the size and orientation of its elliptical cross-sections, positioned according to the material frames along the spine.

---

The `main.R` script is divided into two main sections: **1. Transformation** and **2. Simulation**.

- **Transformation**: This section provides examples for calculating both intrinsic and non-intrinsic means, using intrinsic and non-intrinsic transformations between two e-tubes represented by their corresponding *elliptical tube representations* (ETReps), as discussed in the main manuscript.

- **Simulation**: This section provides an example of ETRep simulation, as discussed in the article's Supplementary Materials. 

---

Please read the **Code_Description.pdf**

---

## Images from the article
![Fig1](https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/8afe4bf2-bd44-4a25-97f2-8ff6d6a18066)
![Fig2](https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/c59f7a7a-64d2-478a-ac87-7d2b349ab0cc)
![Fig3](https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/9baf836d-ebd9-4480-9ab1-20704e77054a)


### A depiction of the simulated ETReps and their mean shape.
<img src="https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/d5122c87-bead-4ebe-9c75-58d03f72b1de" width="700">

### Visualization of a valid transformation without any self-intersections
![Fig5](https://github.com/MohsenTaheriShalmani/Elliptical_Tubes/assets/19237855/5fd1b731-ea15-4cf3-9884-635b6989e735)




