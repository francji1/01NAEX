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

## Submitting from Google Colab and the GitHub website (no git needed)

You only need a GitHub account and a web browser.

**Once per semester: create your fork**

1. Sign in to GitHub and open `https://github.com/francji1/01NAEX`.
2. Click **Fork** (top right) and then **Create fork**. You now have your own copy `https://github.com/<your-login>/01NAEX`.

**Every homework**

1. **Download the notebook from Colab.** In Colab choose *File → Download → Download .ipynb*. Rename the downloaded file
   to `01NAEX_ExNN_HW_<Surname>.ipynb` (no spaces, no diacritics), for example `01NAEX_Ex04_HW_Novak.ipynb`.
2. **Update your fork.** Open your fork `https://github.com/<your-login>/01NAEX`. If GitHub shows
   *This branch is N commits behind francji1/01NAEX:main*, click **Sync fork** and then **Update branch**.
   Never click *Discard commits* if you have work in your fork that has not been merged yet.
3. **Upload the file.** In your fork open the `HW` folder, click **Add file → Upload files**, drag your notebook in,
   write a short message such as `Ex04 Novak`, keep *Commit directly to the main branch* selected and click
   **Commit changes**. Check that the file is inside `HW/`, not in the repository root.
4. **Open the pull request.** Go back to the front page of your fork. Click **Contribute → Open pull request**
   (or the **Compare & pull request** button). Check the header: base repository `francji1/01NAEX`, base `main`
   ← head repository `<your-login>/01NAEX`, compare `main`. The list of *Files changed* must contain only your new
   notebook. Click **Create pull request**.

That is all. You can follow the state of your pull request at `https://github.com/francji1/01NAEX/pulls`.

**Common problems**

* *The pull request also changes other files.* Your fork was out of date or you edited a file outside `HW/`.
  Close the pull request, do *Sync fork* (step 2), delete the extra files in your fork and open a new pull request.
* *I want to fix my submission before it is merged.* Upload the corrected file with the same name into `HW/` of your fork
  (step 3). The open pull request updates automatically; do not open a second one.
* *I uploaded the file to the wrong folder or with a wrong name.* Open the file in your fork, click the pencil icon
  (*Edit*) and change the path in the file name field, e.g. to `HW/01NAEX_Ex04_HW_Novak.ipynb`, then commit.
* *Upload fails because the file is too large* (the web upload limit is 25 MB). In Colab use
  *Edit → Clear all outputs*, rerun only the cells needed for your answers, and download again.
* *Colab: File → Save a copy in GitHub* also works, but it saves into the repository root and creates many commits; prefer
  the upload described above.
