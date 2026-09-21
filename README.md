# Design and Analysis of Experiments (01NAEX)

Materials for the course **01NAEX - Navrhovani a analyza experimentu** at FJFI CTU in Prague, winter semester 2026/27.
Lectures and exercises: Jiri Franc. Monday 15:00-17:30, room T-112.

## Repository structure

- **`lectures/`**
  Lecture slides (PDF), named `01NAEX_LNN.pdf`. Added weekly as the semester progresses.

- **`code/`**
  Jupyter notebooks for the exercises, named `01NAEX_ExNN.ipynb`. Each notebook ends with an assignment
  that you solve at the end of the class (or at home if you missed the lecture).

- **`HW/`**
  Submissions of the end-of-class assignments. Solve the assignment in a copy of the notebook and open a pull request
  that adds it as `HW/01NAEX_ExNN_HW_<Surname>.ipynb`; see `HW/README.md`.

- **`projects/`**
  The two team projects of the semester: assignments and submissions.

- **`data/`**
  Datasets used in the notebooks. Load them directly from GitHub, for example
  `https://raw.githubusercontent.com/francji1/01NAEX/main/data/Ex02_20.csv`.

## How to run the notebooks

- Open any notebook in Google Colab via the badge at its top, or
- clone the repository and run locally with Python 3.11+ and

  ```bash
  pip install numpy pandas scipy statsmodels matplotlib seaborn scikit-learn
  ```

## Course outline

1. Introduction and simple comparative experiments
2. Single factor experiments, ANOVA
3. Factorial designs
4. Blocking, Latin squares, Graeco-Latin squares, BIBD
5. 2^k factorial designs I
6. 2^k factorial designs II
7. 2^k factorial designs: blocking and center points
8. 2^k fractional factorial designs
9. 3^k factorial designs
10. Random factors and mixed models
11. Nested and split-plot designs
12. Longitudinal data analysis
13. Final project

Main literature: D. C. Montgomery, *Design and Analysis of Experiments*, 8th ed., Wiley, 2012.

Materials from previous years are available in the git history.
