# Detailed review – Team Project 1 (pendulum), task by task

Follows the assignment (`projects/01NAEX_Project1.ipynb`, tasks 1–11 and their bullet points). For each question: what we ask and why, the answer and review of team G, the answer and review of team Bl, and a summary.

- **Team G** = Galloppi, Paleta, Remiš, Veselý (`01NAEX_Project1_Galloppi_Paleta_Remis_Vesely`).
- **Team Bl** = Blažek, Guliev, Linazisoro, Morovicsová (`01NAEX_Project1_Blazek_Guliev_Linazisoro_Morovicsova`).
- "Part A" and "Part B" are parts of the experiment (RCBD and Graeco-Latin squares), not teams.

Numbers not visible in the notebooks (team Bl prints its conclusions via `display(Markdown)`) were recomputed from the teams' CSV files; they are marked "(recomputed)".

**One pendulum instead of three:** both teams used a single pendulum and changed its length. The instructor allowed this to speed up the measurements, so it is not a mistake in itself. What is assessed is whether the team **acknowledged** that the experiment is therefore not fully randomized, and whether it **recorded the actual run order**, so that time trends and the error structure can be assessed.

---

## Task 1 – Experimental setup

### 1.1 Apparatus, measurement of $L$ and $\theta_0$, timing and $N$

**Question:** describe the pivot, string, rings (mass, size), how length and angle were measured, how timing was done and the number of oscillations $N$.
**Why we ask:** without a precise description we cannot judge systematic errors (length to the centre of mass, the ring correction $R^2/L$, rigidity of the pivot) or reproduce the experiment. The choice of $N$ directly determines precision: a reaction-time error of 0.05–0.2 s is divided by $N$.

**Team G – answer:**
- One pendulum hung from a sponge holder at the whiteboard, string fixed with tape. Steel rings, 1/2/3 pieces, weighed 10.08 / 20.18 / 30.38 g (20.26 g recorded in Part A, the 0.08 g difference is commented).
- Length 0.35 / 0.65 / 1.00 m measured with a tape measure from the edge of the holder to the centre of the rings after every re-tying. Ring correction $R^2/L$ not applied ("below the precision of the tape measure").
- Angle 5°, 30°, 45° set via $d = L\sin\theta_0$; 5° instead of 10° on purpose, to increase the difference.
- $N = 10$ (compromise between time and amplitude decay), phone stopwatch, start at the lowest point after one swing. In 4 runs operator A counted only 9 oscillations; analysed with $N = 9$, noted, nothing deleted.

**Review:**
- The description is complete and concrete, including reasons for the deviations ($N = 10$, 5°). The ring radius is missing (the $R^2/L$ claim cannot be checked; for $R \approx 1$ cm and $L = 0.35$ m it is 0.03 cm, so the claim is probably true), and there is no photo.
- One pendulum instead of three is an allowed deviation. The team states explicitly that, because of re-tying, the length was not fully randomized and that a time trend may be partly confounded with the length effect. **This is exactly what was expected.** They did not, however, think through the consequences for the error term (Task 3).
- Only the **nominal** length went into the CSV, although the text says it was measured after every setting. Task 7 therefore does not use "measured values".
- 5° at $L = 0.35$ m means $d = 3$ cm; an error of ±0.5 cm is ±1°, irrelevant for the angle effect (0.05 %). Fine.

**Follow-up questions:** What is the ring radius, and how much would $R^2/L$ change $L$ at 0.35 m? Where are the actually measured lengths?

**Team Bl – answer:**
- Pendulum hung from a rack made as rigid as possible; the string length was changed between 0.35, 0.65 and 1.00 m ("We changed the length of the string"), i.e. also a single pendulum. Rings 10.14 / 20.22 / 30.48 g.
- $L$ measured from the pivot to the centre of mass of the rings "taking into account the inner radius", but the table says "to the approximate centre of the bob".
- Angles 10°, 30°, 50° from marks on paper.
- $N = 5$ in Part A, $N = 10$ in the pilot and Part B. No reason given.

**Review:**
- The description is more general: pivot and string, ring size and a photo are missing.
- The single pendulum is allowed, but the team **nowhere acknowledges** that changing the length means the experiment is not fully randomized, nor says how often the length was changed.
- **$N = 5$ in Part A is the main weakness of the whole project.** The assignment recommended $N = 20$. With $N = 5$ a reaction error of 0.1 s is 2 % of the period at 0.35 m, and one miscounted oscillation means −20 %. Consequences: $\hat\sigma$ in Part A is 3.3 % of the period (team G 0.9 %), and block 3 contains obvious counting errors (Task 4). The deviation is acknowledged but **not justified**.
- Different $N$ in different parts also made the pilot useless for planning Part A (different procedure).
- 3 rings instead of 2 in the pilot and Part A: acknowledged, irrelevant for the results.

**Follow-up questions:** Why $N = 5$? What percentage of the period is a 0.1 s error at $N = 5$ and $L = 0.35$ m? How exactly did you measure $L$ ("approximate centre")? How many times did you reset the length?

**Summary 1.1:** Both teams used the allowed single pendulum. Team G described the apparatus better, justified its deviations and acknowledged the restricted randomization. Team Bl did not acknowledge it and chose $N = 5$, which destroyed the precision of Part A. Both should have recorded the actually measured lengths and the ring radius.

### 1.2 Nuisance factors and how they are handled (block, randomize, keep constant)

**Question:** list the expected nuisance factors and say for each whether it is blocked, randomized or kept constant.
**Why we ask:** this is the core of L01 (three principles: replication, randomization, blocking) and L03 ("block what you can, randomize what you cannot").

**Team G – answer:** a table of six factors: operator (block in A, rows in B), time drift (randomization, in B also the square factor), releaser (release from rest at the mark), amplitude decay (constant $N$), errors in $L$ and $\theta_0$ (measured after every change), pivot movement and air currents (constant).

**Review:** a model answer, exactly as in L01/L03. Inconsistency: for drift they claim randomization, which they restricted in Part A and did not actually perform in Part B (Task 3). They acknowledge this for Part A only.

**Follow-up question:** Which factor in your table was in the end not handled by randomization?

**Team Bl – answer:** operator (block in A, row in B); air, friction at the pivot, string stretching, release technique "minimized by using the same equipment and procedure"; "timing habits, damping and counting errors can affect the results".

**Review:** the factors are listed, but the assignment "block / randomize / constant" is missing for each of them. **Randomization and time drift are not mentioned at all**, although the pilot shows a strong trend (Task 2.1) and changing the length restricts randomization. Counting errors are named but nothing is done against them.

**Follow-up question:** How did you protect the experiment against a time trend (learning, fatigue)?

**Summary 1.2:** Team G answered correctly and completely. Team Bl only listed factors; the principle of randomization is missing.

### 1.3 Which factors are nuisance factors in Part A and which in Part B

**Why we ask:** to check that students understand that the same variable can be a nuisance factor in one design and a factor of interest in another (mass).

**Team G:** in A the operator (block) and the run order; mass constant. In B the operator in rows and the square as a second blocking factor; mass is a factor of interest (because of Task 8). **Correct.**

**Team Bl:** only implicitly (operator as block in A, rows in B). No explicit answer.

**Summary 1.3:** only team G answered correctly.

---

## Task 2 – Pilot study and sample size

### 2.1 Pilot data, $\bar T$, $\hat\sigma$ and comparison with theory

**Question:** 10 runs by one operator at 0.65 m, 30°, 2 rings; compute $\bar T$, $\hat\sigma$ and compare with `theory_period()`.
**Why we ask:** $\hat\sigma$ is the input for the power calculation (L01, L03). Comparison with theory is the first check for systematic errors of the apparatus.

**Team G – answer:** 7 separate runs by operator A + 3 Part A runs at the same setting (operators C, B, A). $\bar T = 1.6428$ s, $\hat\sigma = 0.00957$ s, theory 1.6455 s (difference 0.16 %).

**Review:**
- Calculation and comparison correct; agreement with theory is excellent.
- The pilot is **not** "one operator, 10 runs": it mixes 7 consecutive runs of operator A with 3 runs of three different operators. The team says so openly.
- Chronology: runs 8–10 belong to Part A. Formally, the plan $b = 3$ was based on data that were already part of the main experiment. In practice it does not matter ($b = 3$ is the minimum required), but it is worth asking.
- $\hat\sigma$ from 10 runs is uncertain: the 95 % CI is (0.0066; 0.0175) s. Team G reports this in Task 11 – correct.

**Follow-up question:** When exactly did you decide on $b = 3$: before run 8 or after run 10?

**Team Bl – answer:** 10 runs, 3 rings, $N = 10$. $\bar T = 1.5827$ s, $\hat\sigma = 0.0290$ s, theory 1.6455 s, the mean is 3.8 % below theory (recomputed).

**Review:**
- Calculation correct. But the pilot used $N = 10$ and Part A $N = 5$, so it does not represent the Part A procedure. The team says so and adds a scenario "SD doubled for $N = 5$". A reasonable reaction, but repeating the pilot would have been better.
- **Undetected trend:** $T$ increases in measurement order from 1.54 to 1.63 s (Spearman 0.87, $p = 0.001$). $\hat\sigma$ is therefore inflated by drift, and the −3.8 % deviation from theory is partly operator learning. The team does not comment on the deviation from theory at all.
- The note "Corrected pilot" in the data is not explained.

**Follow-up questions:** Plot the pilot in measurement order. What do you see? What does "Corrected pilot" mean? Why is the pilot 3.8 % below theory?

**Summary 2.1:** Team G has a good pilot (with the mixture of sources acknowledged). Team Bl's pilot has a different $N$, a trend and a large deviation from theory, none of which was analysed.

### 2.2 Power of the $F$-test for the angle as a function of the number of blocks $b$

**Question:** target = theoretical difference 10° vs 30° at the middle length, least favourable configuration, noncentral $F$; choose $b \ge 3$. What to do if the power is too low?
**Why we ask:** planning the size of an experiment from a pilot is a key skill from L01 and L03 ($\lambda = n D^2/(2\sigma^2)$, $n = 3b$ per level, $df_E = 8(b-1)$).

**Team G – answer:** $\lambda = 3bD^2/(2\hat\sigma^2)$, $df_1 = 2$, $df_2 = 8(b-1)$, correctly derived. Power 92 % for $b = 2$, 99.7 % for $b = 3$. They chose $b = 3$. To increase power: a larger $N$ reduces the reaction error but increases amplitude decay.

**Review:**
- Methodologically exactly as in L03.
- Power is computed with the point estimate $\hat\sigma$. With the upper CI limit (0.0175 s), $b = 3$ would give only 70 % and $b = 2$ 44 % (recomputed). Planning with the upper limit or with a margin is good practice; the team did not mention it.
- The calculation is on the $T$ scale in seconds and for one length. On a shorter pendulum the angle effect is smaller in seconds, so 0.65 m is not the worst case on the $T$ scale. On the $\log T$ scale the effect is the same for all lengths.

**Follow-up question:** What would the power be if the true $\sigma$ were at the upper limit of the pilot interval?

**Team Bl – answer:** the same (correct) formula. Power table for $b = 2$ to 200: 60 % for $b = 6$, 80 % only at $b = 9$, under 20 % in the "$N = 5$" scenario. Explicitly: "It was made after measurement, so it is not evidence of an earlier plan."

**Review:**
- Formula and interpretation of power are correct; the explanation of what 80 % power does and does not mean is very good.
- **But the task was not done:** $b = 6$ was not derived from the pilot and, by their own calculation, is underpowered. The actual power for the 10° vs 30° target was 21 % (Task 11).
- On "what else could you change" they answer well (longer timing, better equipment, wider angle range), but did the opposite themselves (shorter timing).

**Follow-up question:** How did you choose 6 blocks?

**Summary 2.2:** Both use the correct formula. Only team G planned as required. Team Bl honestly admitted that the calculation is post hoc.

### 2.3 Error degrees of freedom of one and two Graeco-Latin squares

**Why we ask:** to understand the saturation of a $3\times3$ square (L04): 9 observations, 1 + 4·2 = 9 parameters.

**Team G:** one square 0 df ($8 - 4\cdot 2 = 0$), two squares 6 df ($17 - 1 - 4 - 3\cdot2$), with a breakdown by factor. **Correct.**

**Team Bl:** 9 parameters for 9 observations → 0 df; two squares 12 parameters for 18 → 6 df, assuming the same effects in both squares and no interactions. **Correct**, with the assumptions stated.

**Summary 2.3:** both correct.

### 2.4 Final plan before the main experiment

**Why we ask:** the plan should be fixed before the data, otherwise the analysis may be adapted to the results.

**Team G:** $L \in \{0.35, 0.65, 1.00\}$, $\theta_0 \in \{5°, 30°, 45°\}$, $b = 3$, $N = 10$, roles. **Done.** (No mention of the single pendulum and the order of lengths, which later turned out to be the main limitation.)

**Team Bl:** no plan before the measurements; Section 3 explicitly generates a plan "for a repeat experiment". **Not done**, acknowledged.

**Summary 2.4:** only team G.

---

## Task 3 – Randomized run sheets

### 3.1 Part A: random order within each block, with a fixed seed

**Why we ask:** a block in an RCBD must contain all treatments in random order (L03). A fixed seed makes the plan verifiable.

**Team G – answer:** no seed. They show the actual order: first all runs at 0.65 m, then 0.35 m, then 1.00 m; within a length the angles almost always ascending; the three operators measured the same setting one after another, only the order of operators was drawn at random. The deviation is acknowledged in Task 1 (Limitations).

**Review:**
- Acknowledged and justified (re-tying takes time), and the actual order is recorded in the `run` column. With the allowed single pendulum this is **the required minimum, and the team met it**.
- Grouping by length was necessary, but three consecutive runs of the same combination were not. The order of angles and operators within a length could have been fully randomized.
- Consequences not thought through:
  - The time trend is confounded with length and, within a length, with angle.
  - The length was set **once** per level. The three "replicates" share the same tying and the same measurement of $L$. The effect of $L$ is therefore unreplicated, and the residual contains only the timing and release error, not the error of setting the length. This is a split-plot structure (L11): the CIs for $\beta_1$ and $g$ are probably too narrow. Consistent with this, $g$ from Part A differs by length (10.04 / 9.86 / 9.88) more than the interval suggests.
  - Block = operator, but operators alternate run by run. A "block" is therefore not a contiguous session; blocking can capture nothing but the personal timing style.

**Follow-up questions:** Are three consecutive runs of the same combination independent replicates? What does the residual measure? How many independent length settings do you have?

**Team Bl – answer:** a plan with seed `20261026` for 6 blocks, but described as "an example plan for a **repeat experiment**". Actual order not recorded (`run` empty; `source_order` is the row order of the paper protocol: within a block $L$ descending, angle ascending).

**Review:**
- Task not done. A plan generated after the measurements proves nothing.
- The team changed the length of one pendulum, so the measurements were not fully randomized, but it **does not acknowledge this** and **did not record** the actual order. These are exactly the two things that were mandatory with the allowed single pendulum.
- The protocol row order ($L$ descending, angle ascending within a block) suggests that the length was reset in each block. If so, the length effect is replicated across blocks (split-plot with length as the whole-plot factor), which is better than in team G. Without the recorded order this cannot be verified.
- Independence of runs and protection against a time trend therefore cannot be assessed. The team does not raise the question at all.

**Follow-up questions:** In what order did you actually measure? How many times did you reset the length? Was the order of angles within a block random?

**Summary 3.1:** The single pendulum was allowed for both teams. Team G acknowledged that the experiment is not fully randomized and recorded the actual order; it met the minimum, although it did not think through the consequences for the error. Team Bl neither acknowledged nor recorded it. Correct approach: randomize the order of lengths in each block and the order of angles within a length (`rng.permutation`), acknowledge the restriction and record the order in `run`.

### 3.2 Part B: two independently randomized squares, `random_graeco_latin_3()`

**Why we ask:** randomizing rows, columns, symbols and run order protects the square against systematic error (L04).

**Team G – answer:** seeds `31273448` and `70415791`; check "SUCCESS: The generated design squares match the recorded Part B data perfectly!" and matching run order.

**Review (serious):**
- Sorted by `order`, the runs in **both** squares are exactly lexicographic: 1, 1, 1, 2, 2, 2, 3, 3, 3 rings and within that length 0.35, 0.65, 1.00. The probability of such an order is $1/9! \approx 2.8\cdot10^{-6}$ per square; among 20 000 trial seeds it never occurred. The seeds were almost certainly searched for so that the "random" plan matched a convenient (or already executed) order.
- Consequence: mass is **completely confounded with position in time** in both squares (1 ring = runs 1–3, 3 rings = runs 7–9). The mass effect estimate +1.0 % (Tasks 6 and 8) cannot be distinguished from drift within the square.
- The "SUCCESS" check only verifies that the data match the plan, not that the plan is random.

**Follow-up questions:** How did you choose the seeds? What is the probability that a random order follows exactly the number of rings and the length? What does that mean for the mass effect?

**Team Bl – answer:** two squares with seeds `20261027`, `20261028`, again "for a repeat". The actual squares in the data are valid (orthogonality checked in Task 4) but differ from the generated ones.

**Review:** task not done. We do not know whether the actual square was randomized or in what order it was measured. In Part B the length changes in every run, so a random order is laborious with a single pendulum; all the more reason to state how the measurements were actually done.

**Follow-up question:** How did you choose the actual layout of both squares?

**Summary 3.2:** neither team has a documented randomization of Part B. For team G, moreover, randomization is claimed that did not actually take place. Correct approach: generate the plan **before** the measurements with any seed and measure in that order, even if inconvenient.

### 3.3 Actual order in the `run` column

**Team G:** `run` 1–52 filled in. **Done.** `date_time` empty.
**Team Bl:** `run` and `date_time` empty. **Not done.**
**Summary 3.3:** only team G. Without `run`, the diagnostics in Task 9 are impossible.

---

## Task 4 – Data and descriptive analysis

### 4.1 Data check (runs per cell, block, square, missing values, notes)

**Why we ask:** data errors must be found before modelling, not after.

**Team G:** `info()`, counts of notes, cross-tables of runs per cell × block and square × operator × rings. Commented: only `date_time` missing, 7 + 3 pilot runs, 4 runs with $N = 9$. **Correct and complete.**

**Review G:** all 4 runs with $N = 9$ belong to operator A in Part B. Two of them (2 rings, 0.35 m) are 1.9 % and 2.4 % above theory, more than the other runs (recomputed). The adjustment of $N$ may therefore be inaccurate and affects the operator effect.
**Follow-up question G:** How did you find out that the operator counted only 9 oscillations?

**Team Bl:** automatic `assert` checks (counts, positive values, block balance, orthogonality of both squares), table of missing metadata. "All 82 supplied readings are retained, including the low A readings for group C in repetition 1."

**Review Bl:**
- The structural checks are excellent (the most systematic of the two teams).
- They noticed the low values in block 3 but **did not investigate**. For example $L = 0.65$ m, 50°: $t_5 = 6.98$ s corresponds to 4.1 actual periods, almost certainly a counting error. The ratio $T$/theory in block 3 is 0.917 on average (min 0.822). Correct approach: check the protocol, mark in `note`, analyse with and without these points.

**Follow-up question Bl:** How many periods correspond to $t_5 = 6.98$ s at 0.65 m? What should be done with such a point?

**Summary 4.1:** Team Bl has better structural checks, team G better handling of notes. Neither investigated suspicious runs in depth.

### 4.2 Tables of means and SDs ($T$ by $L$ and $\theta_0$ in A, by mass in B)

**Team G:** both tables. Comment: 5° and 30° are close (small angles), 45° clearly higher; SD by mass ≈ 0.37 s because of the lengths. **Correct.** The sentence "with increasing number of rings the pendulum gets slower" is speculation; the differences are not checked.

**Team Bl:** both tables (with counts). Comment only on the SD by mass (contains the length effect). **They do not comment on the huge SDs in Part A** (0.03–0.12 s; 0.125 s at 0.65 m and 50° because of block 3). This was already a sign that something was wrong with the data.

**Summary 4.2:** both done. Team Bl missed the signal in the SDs.

### 4.3 Plots: interaction plot on $T$ and $\log T$, $T$ by operator, $T$ (or deviation from theory) in run order

**Why we ask:** the interaction plot shows why the $\log T$ scale is additive; the plot in run order is the only check for a trend.

**Team G:** all four plots (interaction plot of A + B together, boxplot by operator, $T$ vs `run`). Comment: "parallel lines … no interaction on either scale"; no difference between operators; the trend check is moved to residuals in Task 9 because $T$ is dominated by length.

**Review G:** complete. The claim "parallel on both scales" contradicts theory and their own Task 5a: on the $T$ scale the angle effect grows with length (0.047 s at 0.35 m, 0.079 s at 1 m). The difference is small in the plot, but the comment should say what theory predicts. They correctly recognized that $T$ vs `run` without subtracting theory shows nothing.

**Team Bl:** interaction plot (A only) on both scales, boxplots by operator group and by square/row, **deviation from theory vs length** (a nice plot, showing systematically short periods). No plot in run order (order unknown).

**Review Bl:** good plots, but the comment does not use what they show: the deviation from theory is systematically negative and concentrated at operator C. The run-order plot is missing.

**Summary 4.3:** team G complete, team Bl without the run-order plot. Both could have linked the plots to theory better.

---

## Task 5 – Part A: factorial ANOVA in blocks

### 5.1 Model $T \sim \text{block} + L\times\theta_0$ on $T$ and $\log T$; why the interaction disappears on $\log T$; choice of scale

**Why we ask:** to understand that interaction depends on the scale (L03, multiplicative model → log), and to choose the scale for a design without interactions (Part B).

**Team G – answer:**
- On $T$: interaction $F = 2.36$, $p = 0.097$; on $\log T$: $F = 0.25$, $p = 0.905$. Length and angle highly significant on both scales. Block not significant ($p = 0.20$ and $0.43$).
- Explanation: $T = T_0 f(\theta_0)$, on $T$ the angle adds the same percentage, hence more seconds for a long pendulum; the logarithm turns the product into a sum.
- Why the interaction on $T$ was not detected: its theoretical size is only about 0.03 s; with $\hat\sigma = 0.013$ s and 16 df the power is low; non-significance does not mean absence.
- They prefer $\log T$ (additive, physical meaning of coefficients).

**Review:** excellent, exactly as in L03 and the assignment. Explaining the non-significant interaction through power is exactly what we wanted to hear.

**Team Bl – answer:**
- On $T$: block $F = 8.17$, $p = 2\cdot10^{-5}$; angle $p = 7\cdot10^{-4}$; interaction $p = 0.51$. On $\log T$: interaction $p = 0.46$.
- Explanation $\log T = \log f(L) + \log h(\theta)$; they prefer the log scale. "Our interaction p-values do not establish an interaction, but cannot prove its absence."

**Review:**
- Correct, good explanation of the scale.
- They do not explain why the interaction that theory predicts on $T$ was not found (with $\hat\sigma = 0.054$ s the power for a 0.03 s effect is negligible).
- The significance of blocks ($F = 8.2$) is largely due to the errors in block 3, not to real differences between operators (Task 5.3).

**Follow-up question Bl:** How large is the theoretical interaction in seconds, and did you have a chance to detect it?

**Summary 5.1:** both correct. Team G also argues correctly via power.

### 5.2 Tukey with $MS_E$ and $df_E$ of the model; angles at each length if the effect depends on $L$; comparison with theory

**Why we ask:** `pairwise_tukeyhsd()` fits a one-way model with the wrong $MS_E$ (L02/L03). Comparison with theory is the main test of the result.

**Team G – answer:**
- $\log T$, $n = 9$, HSD = 0.0113: 30−5 +0.88 % (theory 1.69 %), not significant; 45−5 +3.3 % (3.94 %), 45−30 +2.4 % (2.21 %), both significant. All theoretical values inside the simultaneous intervals.
- On $T$ by length with $MS_E$ of the full model: 45−5 significant at all lengths, differences grow with $L$ as theory predicts.
- 30−5 is half of the theoretical value; explained by amplitude decay during the 10 oscillations.

**Review:** correct (helper `tukey_from_model`, correct $MS_E$ and $df_E$). The comparison with theory is explicit and well interpreted ("not significant ≠ no effect").

**Team Bl – answer:**
- Own Tukey via model contrasts (equivalent and correct: half-width $q\cdot SE/\sqrt2$).
- $\log T$ marginally: 30−10 0.0178 (theory 0.0154), not significant; 50−10 0.0456 (0.0466), $p < 0.001$; 50−30 0.0278 (0.0312), $p = 0.049$.
- By length on $\log T$ with a theory column: at 0.35 m, 30−10 is even negative (−0.96 %).

**Review:**
- Technically correct, and the agreement of the marginal differences with theory is **very good** (better than team G).
- A verbal comparison with theory is missing; the "theory" column appears only in the per-length tables.
- The negative 30−10 difference at 0.35 m is not commented; it shows how large the noise is.

**Follow-up question Bl:** Does your marginal 50−10 difference agree with theory? Why is 30−10 at 0.35 m negative?

**Summary 5.2:** both correct. Team G interprets better; team Bl is numerically closer to theory (thanks to the wider range of angles).

### 5.3 Was blocking useful? Relative efficiency of the RCBD vs a CRD

**Why we ask:** L03 (Montgomery): $\hat\sigma^2_{CR} = \frac{(b-1)MS_B + b(a-1)MS_E}{ab-1}$, $R = \hat\sigma^2_{CR}/MS_E$ (optionally with a df correction). The plain ratio of $MS_E$ of the models without and with blocks is a different (incorrect) quantity.

**Team G – answer:** Montgomery's formula + df correction: $R = 1.05$ ($T$), $0.98$ ($\log T$); blocking did not help, on $\log T$ we paid 2 df for nothing. With the caveat that, because of restricted randomization, this is only a rough guide.

**Review:** correct according to L03, with careful interpretation. A model answer.

**Team Bl – answer:** $MS_E(\text{CRD})/MS_E(\text{RCBD}) = 1.80$, slightly less with the df correction; blocking improved precision.

**Review:**
- The formula **does not match L03**. Montgomery's formula gives $R = 1.68$ (recomputed). The conclusion ("blocking helped") is the same.
- Interpretation: the large block effect is mainly block 3 (counting errors) and operator C. Without block 3 the block test is still significant ($p = 0.02$) but weaker. "Blocking helped" here means rather "the block absorbed gross errors" than that operators had stable different habits.

**Follow-up question Bl:** What formula for relative efficiency was in L03, and why does it differ from the plain ratio of $MS_E$?

**Summary 5.3:** team G correct. Team Bl has the right conclusion with the wrong formula.

---

## Task 6 – Part B: Graeco-Latin squares

### 6.1 Factor model for two squares with ANOVA table and df; the same model for one square

**Why we ask:** to check understanding of the square structure, rows within squares and saturation (L04).

**Team G – answer:**
- Check of the square properties, ANOVA with df breakdown (1 + 4 + 2 + 2 + 2 + 6 = 17). Length $F = 7396$, angle $F = 23.3$, $p = 0.0015$, mass $F = 3.93$, $p = 0.081$, rows $p = 0.12$. $\hat\sigma = 0.7$ %.
- Length estimates +0.300 and +0.517 vs theory 0.309 and 0.525; angle 45° +2.9 % (theory 3.9 %), 30° +1.0 % (1.7 %).
- One square: 9 parameters, 0 df, $R^2 = 1$; they show that `anova_lm(typ=2)` fails and `typ=1` returns a meaningless $F = 0$.

**Review:** excellent. Showing the software behaviour for a saturated model goes beyond the requirements.

**Team Bl – answer:**
- ANOVA on $\log T$: length $F = 672$; angle $F = 3.81$, $p = 0.085$; mass $p = 0.35$; rows $p = 0.14$.
- One square: rank 9, 0 df, "a perfect fit is not evidence of a perfect physical model".

**Review:**
- Correct.
- **They do not comment that the angle is not significant in Part B** ($p = 0.085$), although it is in Part A. Reason: $\hat\sigma$ in Part B is about 2.4 % (recomputed), three times that of team G, with only 6 df. The 50−10 estimate is 0.039 vs theory 0.047.

**Follow-up question Bl:** Why is the angle not significant in Part B when it is in Part A?

**Summary 6.1:** both correct. Team G deeper and with comparison to theory.

### 6.2 The same model on $T$; comparison of the $F$-tests for mass and operator on both scales

**Why we ask:** on the $T$ scale the $L\times\theta$ interaction is aliased with mass and operator, so their tests on $T$ are biased.

**Team G:** table for both scales; $F$ slightly larger on $\log T$, same decisions; "those tests are the wrong ones" (meaning on $T$).
**Review G:** correct; the wording "those tests" is unclear. They could have added that the bias is negligible at their precision (see 6.3: spurious mass SS $1.7\cdot10^{-6}$).

**Team Bl:** table of $F$ and $p$ for mass and rows on both scales. Only a general verbal comparison ("mass and row conclusions still depend on this assumption").
**Review Bl:** formally done; interpretation of the difference missing.

**Summary 6.2:** both done; neither quantified the bias explicitly (team G indirectly in 6.3).

### 6.3 Aliasing in a $3\times3$ square: what it means for $L\times\theta_0$ and for conclusions about mass and operators on the $T$ scale; why $\log T$ is the right scale

**Why we ask:** the key property of a Graeco-Latin square (L04): the interaction of two factors is completely confounded with the main effects of the other two.

**Team G – answer:**
- Verbally: the 8 df of a square are used up; the interaction (4 df) must lie in the space of rows and mass.
- **Numerically:** projection of the interaction space onto the row + mass space (rank 4, residual $3\cdot10^{-15}$).
- Noise-free theoretical periods fitted by the model: spurious mass and operator effects appear on $T$ and vanish to machine precision on $\log T$.

**Review:** outstanding, the best part of both projects. The only thing missing: aliasing also concerns other interactions, e.g. mass × angle (a lighter bob decays faster, so the angle effect depends on mass). That one is aliased with length and operator, and the logarithm does not remove it.

**Team Bl – answer:** verbally: the four length × angle interaction terms coincide exactly with the four mass and row terms; an interaction can look like a mass or operator effect; a second square does not estimate the interactions; the log scale is supported by theory but does not remove damping or systematic errors.

**Review:** correct and concise. The remark that the log does not remove damping is exactly the point team G missed.

**Follow-up question (both):** Which other interactions can the square hide, and does the logarithm remove them? (Mass × angle due to damping.)

**Summary 6.3:** both correct; team G exceptionally thorough.

---

## Task 7 – Numeric model, the exponent of $L$ and the estimate of $g$

### 7.1 Regression of $\log T$ on $\log L$, $x(\theta_0)$, $m$, operator and block/square; tests $\beta_1 = 1/2$, $\beta_2 = 1$; why $x(\theta_0)$ instead of $\theta_0^2$

**Why we ask:** moving from the factor model to the physical model (regression in ANOVA, L02); testing against theoretical values, not against zero.

**Team G – answer:**
- 45 runs, operator + session (A, B1, B2), both in Sum coding, $R^2 = 0.9985$, $\hat\sigma = 0.9$ %.
- $\hat\beta_1 = 0.5014$ (0.495; 0.508), test of 1/2: $p = 0.65$. Part A alone: 0.508, $p = 0.056$.
- $\hat\beta_2 = 0.812$ (0.636; 0.987), test of 1: $p = 0.036$, rejected. Explained by amplitude decay.
- $x(\theta_0)$ vs $\theta_0^2$: $SS_E$ 0.00312 vs 0.00313, table of the difference (0.09 % at 50°), explanation why $\beta_2 = 1$ is a physical hypothesis only with $x$.

**Review:**
- Correct and complete, good coding.
- The angle result agrees with the physics of a real pendulum: decay reduces the mean amplitude during the timing, so the effect is smaller than for the release angle.
- The CI for $\beta_1$ is probably too narrow because of the unreplicated length setting (Task 3.1). The borderline $p = 0.056$ from Part A points the same way.

**Follow-up question G:** How would you verify or account for the amplitude decay (e.g. measure the amplitude at the end)?

**Team Bl – answer:**
- 72 runs, 12 strata (6 blocks of A, 6 rows of B) in Sum coding, mass centred only in B (zero in A, absorbed by the strata).
- $\hat\beta_1 = 0.489$ (0.472; 0.506), $p = 0.19$; $\hat\beta_2 = 0.93$ (0.55; 1.31), $p = 0.71$ ($p$ recomputed).
- $x$ vs $\theta_0^2$: only the claim "describes 50° better", no demonstration.

**Review:**
- A well-thought-out model (handling the fixed mass in A is elegant and correct).
- "$\beta_2$ compatible with 1" is true, but the interval (0.55; 1.31) is so wide that the test says almost nothing. This should be stated.
- The argument for $x$ vs $\theta_0^2$ is missing.

**Follow-up question Bl:** What is the smallest $\beta_2$ your experiment would still not reject? Why is $x(\theta)$ better than $\theta^2$?

**Summary 7.1:** both have a length exponent consistent with 1/2. Team G estimated the angle precisely and found a physically explainable deviation; team Bl's angle agrees with theory only because of imprecision.

### 7.2 Numeric model on one square: residual df compared with the factor model

**Why we ask:** a numeric factor costs 1 df instead of 2, so even one square allows a test.

**Team G:** 9 runs, 6 parameters, 3 df (factor model 0 df), coefficients shown; "each numeric factor costs one df instead of two". **Correct.**
**Team Bl:** rank 6, 3 df; "the gain comes from assuming a particular response shape, not from extra data". **Correct**, with a good remark about the price.
**Summary 7.2:** both correct.

### 7.3 Estimate of $g$ with fixed $\beta_1 = 1/2$, $\beta_2 = 1$; CI by transformation; coding; cause if 9.81 is outside

**Why we ask:** transforming the interval, the meaning of the intercept under the coding, and above all critical thinking about systematic errors.

**Team G – answer:**
- $\hat g = 9.84$ (9.78; 9.91), 9.81 inside. Part A alone 9.92 (9.85; 10.00), just outside; Part B 9.80 (9.71; 9.90).
- Demonstration that treatment coding would give an intercept corresponding to 10.03 (a different quantity).
- $g$ by length in Part A: 10.04 / 9.86 / 9.88. Largest for the shortest pendulum → additive error in $L$ (0.3–0.8 cm) or in $t_N$ (−0.04 to −0.14 s); distinguished by the $1/L$ vs $1/\sqrt L$ dependence.

**Review:**
- Excellent, exactly what the task asked for.
- A check by operator (recomputed) would add: A 9.78, B 9.94, C 9.91, so part of the deviation is due to timing.
- The intervals do not include the uncertainty of the length; the team knows this.

**Team Bl – answer:**
- $\hat g = 10.25$ (10.08; 10.43), 9.81 outside. Part A 10.28, Part B 10.22 (recomputed).
- The interval describes only statistical uncertainty, not systematic errors (well said).
- Possible causes listed in general (length, counting, timing, $N$, angle, damping); "these data cannot identify the cause".

**Review:**
- Transformation and coding correct; the careful interpretation of the interval is good.
- **"The data cannot identify the cause" is not true; the data largely can:**
  - $g$ by length in Part A increases: 10.14 / 10.29 / 10.41. A length error would give the largest deviation at the shortest pendulum, i.e. the opposite pattern. It is not the length.
  - $g$ by operator: A 9.85, B 10.07, **C 10.91** (recomputed). The ratio $T$/theory for operator C is 0.942 in Part A and 0.917 in block 3.
  - Without block 3, Part A gives $\hat g = 10.02$.
  - The main cause is therefore timing/counting by operators B and C, especially block 3. Operator A measured periods consistent with theory (ratio 0.996–1.005).

**Follow-up question Bl:** Compute $g$ separately for each operator and each length. Which cause can you exclude?

**Summary 7.3:** team G answered in an exemplary way and its $g$ agrees. Team Bl's $g$ is 4.5 % too high, and the team did not analyse the cause that its own data reveal.

---

## Task 8 – Does mass matter? Equivalence, not significance

### 8.1 Mass effect with a 95 % CI from the factor model (3 − 1 ring) and the numeric model

**Why we ask:** an effect should be described by its size and an interval, not only by a $p$-value.

**Team G – answer:** factor model +1.02 % (−0.02; +2.08), numeric +1.02 % (−0.06; +2.11). The interval contains 0, not rejected.

**Review:**
- Calculation correct.
- Physical context is missing, though interesting: the positive sign is consistent with damping. A lighter bob decays faster, has a smaller mean amplitude and a shorter period. In the data the 1-ring runs at 45° are 2.0 % and 1.1 % below theory, the 3-ring runs −0.7 % and +0.4 % (recomputed). That would be a mass × angle interaction, which is aliased in the square.
- At the same time, mass is confounded with run order (Task 3.2), so +1 % may also be drift.

**Follow-up question G:** Does the +1 % effect have a physical explanation? How would you distinguish it from drift?

**Team Bl – answer:** factor model −2.10 % (−5.43; +1.35), numeric −2.09 % (−5.56; +1.50).

**Review:** correct. The interval is very wide because of the large $\hat\sigma$ in Part B. The negative sign is opposite to what damping would predict, but within the noise.

**Summary 8.1:** both computed correctly. Team G has a precise estimate, but confounded with time; team Bl's estimate is imprecise.

### 8.2 Choice and justification of $\Delta$; TOST, i.e. 90 % CI inside $(-\Delta, \Delta)$

**Why we ask:** an equivalence test is the right answer to "does mass have no effect?" (L01: power, type II error). The margin is chosen **in advance** from a practical requirement.

**Team G – answer:**
- They tried $\Delta = 1$ % and 2 %, justified by $dg/g = 2\,dT/T$ (i.e. $g$ to 2 % or 4 %).
- 90 % CI (+0.19; +1.86) %: equivalence not shown at 1 %, shown at 2 %.

**Review:**
- The TOST method via the 90 % CI is correct.
- **Margin chosen after looking at the data**: they picked the one that passes. The assignment suggested $g$ to 1 %, i.e. $\Delta = 0.5$ %; that one fails.
- 2 % of the period = 4 % in $g$ ≈ 0.4 m/s², six times the width of their own CI for $g$. Calling it a "highly precise margin" is an overstatement.
- **The 90 % CI does not contain zero.** At the 5 % level (one-sided) the data rather indicate a positive effect. The team does not comment on this.

**Follow-up questions G:** When is the margin chosen, and why? What does it mean that the 90 % CI excludes zero?

**Team Bl – answer:** $\Delta = 0.5$ % justified by $g$ to 1 %, admitted to be chosen after the measurements. A full TOST with $t$ and $p$ (TOST $p = 0.85$ and $0.81$): equivalence not shown.

**Review:** correct and honest. A reasonable, single margin. They could have added how many runs would be needed to show equivalence with their $\hat\sigma$.

**Summary 8.2:** team Bl correct. Team G has the right procedure but bad practice (margin chosen by the result) and an overstated conclusion.

### 8.3 The difference between "the mass effect is not significant" and "the mass effect is negligible"

**Why we ask:** this is the point of the whole task and of the remark in the assignment about last year's teams.

**Team G:** correct explanation (failing to reject $H_0$ ≠ proof of zero; equivalence = the whole 90 % CI inside the margin). The wording "it is guaranteed to be too small to matter" is an overstatement (statistics guarantees nothing).
**Team Bl:** brief and correct: "Failure of either equivalence test does not prove mass matters; it means a practically relevant effect cannot be ruled out."

**Summary 8.3:** both understand the concepts. Team G overstated the wording and, more importantly, the practice (8.2).

---

## Task 9 – Model diagnostics

**Question:** for the models of Parts A and B on the chosen scale: residuals vs fitted values, normal Q-Q plot, residuals vs each factor, operator and **run order**; comments and proposals.
**Why we ask:** checking the ANOVA assumptions (L02), above all independence and time trends.

**Team G – answer:**
- Part A: residuals in three groups by length; Q-Q approximately normal, two points in the upper tail at 0.35 m; **heteroscedasticity**: about three times larger spread at 0.35 m, explained by a constant timing error, relatively larger for a short period. Run order: no trend, largest residuals in runs 17–23, when 0.35 m was measured.
- Part B: no problems, no trend in run order, smaller residuals (with caution because of the number of parameters).

**Review:**
- Complete and well interpreted, including the physical explanation of the heteroscedasticity.
- They did not realize that in Part B **the residuals-vs-run-order plot cannot show drift**: the order within a square is the same as mass, so any drift would be absorbed entirely into the mass effect estimate and would not appear in the residuals. Similarly in Part A: the cluster of residuals in runs 17–23 cannot be separated from the length effect.
- A remedy for the heteroscedasticity (larger $N$ for the short pendulum, weighted regression) only appears in Task 11 ($N = 20$).

**Follow-up question G:** Could the residuals-vs-run-order plot in Part B reveal drift when the order is identical with mass?

**Team Bl – answer:**
- Six panels (no run order), Cook's distance table, Shapiro–Wilk (A: $p = 0.034$, B: $p = 0.51$).
- Comment: group C in repetition 1 has short periods; a block removes an average shift, not a shift that depends on length; Part B has high leverage, so small residuals prove nothing; these plots cannot verify independence.

**Review:**
- Statistically a very good commentary (leverage, non-additive block effect, caution with Shapiro).
- **Residuals vs run order are missing** (order unknown); requirement not met.
- The detected outliers (Cook's D 0.28 for $t_5 = 6.98$ s) led to no action: neither a check of the protocol nor a sensitivity analysis without them.

**Follow-up question Bl:** How do the conclusions change without block 3? (Answer: $\hat\sigma$ 0.054 → 0.044 s, $\hat g_A$ 10.28 → 10.02.)

**Summary 9:** team G complete, but overestimated what the run-order plot can show. Team Bl has a deeper statistical commentary, but no run-order plot and no sensitivity analysis.

---

## Task 10 – Comparison of the two designs

### 10.1 SE of the angle (50° − 10°) and length effects on $\log T$ in Parts A and B; per-run comparison (variance × number of runs)

**Why we ask:** to understand that both balanced designs have the same per-run efficiency for a main effect: $\operatorname{var}(\hat\tau_i - \hat\tau_j)\cdot n = 2\sigma^2/(n/3)\cdot n = 6\sigma^2$. The difference comes only from $\sigma$, not from the design.

**Team G – answer:** SEs taken from the coefficients `C(theta_deg)[T.45]` and `C(L_m)[…]` of the model **with interaction**: A 0.0076, B 0.0043; per-run ratio B/A = 0.21. Conclusion: "Graeco-Latin square is definitely more efficient … blocking is applied to both operator and mass while in the latter only to operator."

**Review (error):**
- In the model `C(L_m)*C(theta_deg)` with treatment coding, `C(theta_deg)[T.45]` is the **simple effect at $L = 0.35$** ($n = 3$ per cell), not the main effect. Its SE is $\sqrt3$ times larger. The correct SE of the main effect in A is $\sqrt{2\cdot 8.67\cdot10^{-5}/9} = 0.0044$.
- The correct per-run ratio is just the ratio of the $MS_E$: $0.0074^2/0.0093^2 \approx 0.63$, and that is not significant ($F$-test, $p \approx 0.6$).
- The conclusion is **conceptually wrong**: mass is a factor of interest in the square, not a block; the square is not "more efficient per run", it just estimates more factors from the same number of runs.
- The identical SEs for angle and length should have been a warning sign (both are the SE of a single cell).

**Follow-up questions G:** What does the coefficient `C(theta_deg)[T.45]` estimate in a model with interaction? What is the per-run variance of a main effect in the two designs?

**Team Bl – answer:** marginal contrasts via the averaged design matrix: A SE 0.0114, B 0.0141; var·n 0.00701 (A) and 0.00360 (B). The difference is attributed to the different $N$ (5 vs 10) and noise, not to the design.

**Review:** **correct.** The numbers equal $6\,MS_E$ in both parts. They could have stated the general relation var·n = $6\sigma^2$ explicitly.

**Summary 10.1:** only team Bl correct. Team G picked the wrong parameter and drew a wrong conclusion from it.

### 10.2 What can A estimate that B cannot and vice versa; assumptions of each design and whether the data support them

**Why we ask:** the price and the gain of a Graeco-Latin square (research question 4).

**Team G – answer:** A estimates the $L\times\theta$ interaction, not mass; B four main effects from 18 runs, no interactions ("not enough df"). Assumption of A: block × treatment additivity, "supported by RE ≈ 1". Assumption of B: no interactions; violated on $T$, hence $\log T$.

**Review:**
- The split of capabilities is correct (missing: A also estimates operators as blocks).
- "Not enough df" is not the precise reason; the reason is aliasing (Task 6.3, which they demonstrated so well themselves).
- "RE ≈ 1 confirms block × treatment additivity" is a **non sequitur**. RE measures the size of the block effect, not the interaction of block and treatment. That would be shown by residuals by block and treatment or by Tukey's test for non-additivity (L03).
- The B assumption on $\log T$ is supported by theory, but a possible mass × angle interaction (damping) remains (see 8.1).

**Team Bl – answer:** a table: A length × angle with interaction, mass fixed; B mass, no interactions; limitation of A "setting-dependent block effects enter the error estimate", of B "interactions can be mistaken for separate factor effects"; the log scale is supported by theory but cannot be verified.

**Review:** correct and precise, including where a violated assumption ends up in each design.

**Summary 10.2:** team Bl better. Team G has a logical error in the additivity argument.

### 10.3 Only 18 runs for research questions 1–3: which design and why

**Why we ask:** synthesis: question 3 (mass) needs mass as a factor, which the RCBD in Part A does not have.

**Team G:** Graeco-Latin squares, "the superior and only viable choice", because they estimate four effects and "block on operators and mass".
**Review G:** a defensible choice, but the justification repeats the "blocking on mass" error, and "only viable" is an overstatement (e.g. an 18-run factorial with mass and a numeric model would also work).

**Team Bl:** two independently randomized squares plus the numeric model as a complement; interaction checks are given up, and even this may not suffice for ±0.5 % mass equivalence; if interactions were the priority, a replicated full factorial.
**Review Bl:** an excellent, balanced answer.

**Summary 10.3:** both chose squares; team Bl justified it better.

---

## Task 11 – Reflection and conclusions

### 11.1 Actual power for 10° vs 30° with $\hat\sigma$ from the main experiment; did the pilot estimate of $\sigma$ hold?

**Why we ask:** a retrospective check of the planning (L01), lessons for next time.

**Team G:** on $\log T$: $D = x(30) - x(10)$, $n = 9$, $df = 16$ → **82 %** (planned 99.7 %). The pilot underestimated $\sigma$ (0.58 % vs 0.93 %) because it used one operator and one length; the Part A value lies in the pilot's 95 % CI (0.0066; 0.0175) s. 30° vs 5° still was not detected because the observed effect was half of the theory.
**Review G:** exemplary, including the interval for $\sigma$ and the explanation.

**Team Bl:** $\hat\sigma_A = 0.0537$ s vs pilot 0.0290 s (1.85×); power with $b = 6$ is **21 %**. They explicitly reject "observed power" from the measured effect – correct.
**Review Bl:** correct and well informed. The explanation why $\sigma$ is larger ($N = 5$, block 3), which they have elsewhere, is missing here.

**Summary 11.1:** both correct. Team G had a good plan that worked; team Bl had an underpowered design and correctly admitted it.

### 11.2 Answers to the four research questions

**Team G:**
- RQ1: $\beta_1 = 0.501$ consistent with 1/2; $g = 9.84$ (9.78; 9.91), 9.81 inside. They do not mention that Part A alone excludes 9.81 (that is in Task 7).
- RQ2: angle clearly detected, $\beta_2 = 0.81$ smaller than 1, explained by damping. Good.
- RQ3: "We showed that mass has no practically relevant effect" at a 2 % margin, "highly precise". **Overstated** (Task 8.2), and confounded with time.
- RQ4: "extreme efficiency per run by blocking on … Operator and Mass". **Wrong** (Task 10.1).

**Team Bl:**
- RQ1: exponent 0.489, $g = 10.25$ (10.08; 10.43) "conditional on timing and calibration assumptions". They do not say explicitly that 9.81 is outside.
- RQ2: 50° − 10° in Part A +4.67 % vs theory +4.77 % (recomputed), very good agreement.
- RQ3: equivalence not shown; a non-significant test does not establish irrelevance. Correct.
- RQ4: squares study more factors with fewer runs at the price of additivity and 6 df; the factorial estimates the interaction but not mass. Correct.

**Summary 11.2:** team Bl answers more carefully and more correctly (RQ3, RQ4). Team G has better results (RQ1, RQ2) but overstated RQ3 and got RQ4 wrong.

### 11.3 What to change in a repeat with the same time budget

**Team G:** $N = 20$, a pilot with all operators and the shortest length, maximum angle 50–60°, measure $L$ to the centre of the rings. Concrete and correct. **Missing** the main point: how to randomize more even with one pendulum (random order of lengths, reset the length in each block, randomize angles and operators within a length) and a truly random order in Part B.

**Team Bl:** the same $N$ everywhere and a pilot with the same procedure, video or a photogate, careful measurement of $L$, the same release procedure, record anomalies, plan the number of repeats from the precision needed for mass. Correct but more general. **Missing**: acknowledge the restricted randomization, record and randomize the run order, and check the number of oscillations (block 3).

**Summary 11.3:** both proposed sensible improvements of the measurement; neither named the main shortcoming of the design (randomization and run order).

---

## Task 12 – Submission and reproducibility

**Why:** the notebook must run in Colab from top to bottom and read the data from a URL.

**Team G:** reads from the raw URL of the fork (`paletvoj`) with a local fallback; saved with outputs; the CSV has the required columns (`date_time` empty). Minor points: `if "data" not in globals()` (non-standard); an f-string with nested double quotes in Task 5c works only in Python ≥ 3.12 (Colab is fine).

**Team Bl:** first the local CSV, then the fork URL (`CasualMathEnjoyer`); outputs saved; the CSV has extra columns `source_order`, `group`, `code` (fine); `run` and `date_time` empty. Conclusions are printed via `display(Markdown)`, so the numbers are not visible in a text view of the notebook; they render on GitHub. The assignment text and the Colab badge were removed (does not matter).

**Summary 12:** both reproducible. Team Bl did not provide the required CSV content (`run`).

---

## Overall summary by task

| Task | Team G | Team Bl | What to improve (if neither did it) |
|---|---|---|---|
| 1.1 apparatus, $N$ | good; one pendulum (allowed), restriction acknowledged | weaker description; one pendulum not acknowledged; $N = 5$ | record measured $L$ and ring radius |
| 1.2 nuisance factors | **model answer** | no randomization | – |
| 1.3 A vs B | correct | missing | – |
| 2.1 pilot | correct (mixed pilot) | different $N$, trend and −3.8 % not noticed | – |
| 2.2 power, $b$ | **correct, plan from the pilot** | formula correct, $b$ not planned | plan with the upper limit of $\sigma$ |
| 2.3 df of squares | correct | correct | – |
| 2.4 plan in advance | done | not done | – |
| 3.1 run sheet A | restricted randomization, acknowledged, order recorded | not acknowledged, post hoc plan, order unknown | randomize what is possible (order of lengths, angles within a length) |
| 3.2 run sheet B | **searched seeds**, mass = time | post hoc plan | **randomize in advance** |
| 3.3 `run` | yes | no | – |
| 4 data and plots | complete | better structural checks, no run order, block 3 not investigated | investigate suspicious runs |
| 5.1 ANOVA, scale | **excellent** | correct | – |
| 5.2 Tukey, theory | correct, good interpretation | correct, closer to theory | – |
| 5.3 RE | **as in L03** | different formula (1.80 vs 1.68) | – |
| 6.1 squares, one square | excellent | correct, angle in B not commented | – |
| 6.2 $T$ vs $\log T$ | correct | formal | quantify the bias |
| 6.3 aliasing | **outstanding, numerical** | correct, verbal | mention mass × angle (damping) |
| 7.1 regression | correct, $\beta_2 = 0.81$ explained | correct, weak test of $\beta_2$ | – |
| 7.2 one square numeric | correct | correct | – |
| 7.3 $g$ | **exemplary**, 9.81 in CI | $g$ outside, cause not analysed | analysis by operator |
| 8.1 CI of mass | correct | correct | – |
| 8.2 TOST | margin after the data, overstated | **correct** | – |
| 8.3 not significant vs negligible | correct, overstated wording | correct | – |
| 9 diagnostics | complete | no run order, no sensitivity analysis | a run-order plot cannot show drift confounded with a factor |
| 10.1 per run | **error** (simple effect) | **correct** | – |
| 10.2 assumptions | non sequitur about RE | correct | – |
| 10.3 18 runs | defensible, wrong justification | excellent | – |
| 11.1 actual power | exemplary | correct | – |
| 11.2 research questions | RQ3 overstated, RQ4 wrong | careful and correct | – |
| 11.3 improvements | concrete | more general | both: randomization and recording the order |

**Team G** ran the experiment more carefully (pilot → power → plan, `run`, notes), reached a precision of 0.9 % and results consistent with theory (exponent, $g$, angle with a physically explainable deviation). The analysis is in places exceptional (aliasing, $g$ by length, RE). They acknowledged the restricted randomization with one pendulum and recorded the order. Weaknesses: in Part B they claim a randomization that the searched seeds contradict; consequences of the unreplicated length setting not thought through; the error in Task 10 and overstated conclusions about mass.

**Team Bl** has statistically more careful and more correct interpretations (comparison of designs, TOST, assumptions). The weaknesses are in the execution: $N = 5$, plan made after the measurements, unacknowledged restricted randomization and missing run order, uninvestigated counting errors in block 3, and $g$ 4.5 % off without analysing the cause that its own data reveal (operator C).
