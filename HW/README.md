# Homework

Each exercise notebook in `code/` ends with an assignment. If you attend the class, you solve it at the end of the lesson;
if you missed the class, you solve it at home. Either way, submit it here.

## How to submit

1. Fork the course repository `https://github.com/francji1/01NAEX`.
2. Solve the assignment in a copy of the exercise notebook and save it into this folder as
   `01NAEX_ExNN_HW_<Surname>.ipynb`, for example `01NAEX_Ex01_HW_Novak.ipynb`.
   One file per student; for a pair join the surnames with an underscore, for example `01NAEX_Ex01_HW_Novak_Svoboda.ipynb`.
3. Open a pull request against the `main` branch of `francji1/01NAEX` before the next lecture.

Keep the file name pattern and do not change other files in the pull request. If the notebook is larger than a few MB,
clear the largest outputs before submitting.

## Submitting from Google Colab (no git, no Python on your computer)

You only need a GitHub account and a web browser. Work in Colab, save your notebook from Colab into your fork on GitHub,
and open the pull request on the GitHub website.

**Once per semester: create your fork**

1. Sign in to GitHub and open `https://github.com/francji1/01NAEX`.
2. Click **Fork** (top right) and then **Create fork**. You now have your own copy `https://github.com/<your-login>/01NAEX`.

**Every homework**

1. **Update your fork.** Open your fork `https://github.com/<your-login>/01NAEX`. If GitHub shows
   *This branch is N commits behind francji1/01NAEX:main*, click **Sync fork** and then **Update branch**.
   Never click *Discard commits* if you have work in your fork that has not been merged yet.
2. **Open the exercise in Colab** with the *Open in Colab* badge at the top of the notebook in `code/` and solve the assignment.
   Run all cells so that the outputs are saved.
3. **Save it into your fork.** In Colab choose *File → Save a copy in GitHub*. The first time, Colab asks you to authorize
   access to your GitHub account; allow it. In the dialog set
   * **Repository:** `<your-login>/01NAEX` (your fork, not `francji1/01NAEX`),
   * **Branch:** `main`,
   * **File path:** `HW/01NAEX_ExNN_HW_<Surname>.ipynb`, for example `HW/01NAEX_Ex04_HW_Novak.ipynb`
     (the `HW/` folder is part of the path; no spaces, no diacritics). **Do not keep the default path**, it would save
     the file into the repository root as `01NAEX_Ex04.ipynb`,
   * **Commit message:** for example `Ex04 Novak`,
   * **Include a link to Colab:** untick it,

   and click **OK**. Open your fork on GitHub and check that the file is in `HW/` with the right name.
4. **Open the pull request.** On the front page of your fork click **Contribute → Open pull request**
   (or the **Compare & pull request** button). Check the header: base repository `francji1/01NAEX`, base `main`
   ← head repository `<your-login>/01NAEX`, compare `main`. The list of *Files changed* must contain only your new
   notebook. Click **Create pull request**.

That is all. You can follow the state of your pull request at `https://github.com/francji1/01NAEX/pulls`.

**Alternative: upload the file on the GitHub website.** If saving from Colab does not work, choose
*File → Download → Download .ipynb* in Colab, rename the file to `01NAEX_ExNN_HW_<Surname>.ipynb`, open the `HW` folder
in your fork, click **Add file → Upload files**, drag the notebook in, write a short message such as `Ex04 Novak`, keep
*Commit directly to the main branch* selected and click **Commit changes**. Then continue with step 4.

**Common problems**

* *The pull request also changes other files.* Your fork was out of date or you saved a file outside `HW/`.
  Close the pull request, do *Sync fork* (step 1), delete the extra files in your fork and open a new pull request.
* *I want to fix my submission before it is merged.* Save the corrected notebook again with the same path
  `HW/01NAEX_ExNN_HW_<Surname>.ipynb` (step 3 or the upload). The open pull request updates automatically;
  do not open a second one.
* *The file ended up in the wrong folder or with a wrong name.* Open the file in your fork, click the pencil icon
  (*Edit*) and change the path in the file name field, e.g. to `HW/01NAEX_Ex04_HW_Novak.ipynb`, then commit.
* *My fork does not appear in the Colab dialog.* Close the dialog, reload Colab and try again; if it is still missing,
  check that the fork exists under your account, or use the upload alternative.
* *The file is too large* (the web upload limit is 25 MB). In Colab use *Edit → Clear all outputs*, rerun only the cells
  needed for your answers, and save again.
