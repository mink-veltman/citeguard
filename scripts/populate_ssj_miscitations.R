setwd("/Users/minkveltman/Library/Mobile Documents/com~apple~CloudDocs/cloud/MTPRstudio")

library(dplyr)
library(readr)
library(digest)
library(httr2)
library(base64enc)
library(stringr)

source("app/R/taxonomy.R")
source("app/R/utils.R")
source("app/R/db.R")

sync_db_from_github()
cat("Synced from GitHub. Current row count:", nrow(read_reports_db()), "\n\n")

SSJ_DOI      <- "10.1525/collabra.33267"
ROLE         <- "author"
ORCID        <- ""
T_AUTHORS    <- "Lakens"
T_LEAD       <- "Lakens"
T_KEY        <- make_author_key(extract_last_names_from_ref_authors("Lakens"))
T_YEAR       <- "2022"
T_TITLE_CLUE <- "Sample size justification"
T_TITLE_KEY  <- make_title_key("Sample size justification")

row <- function(citing_doi, codes, titles, quote, why) {
  data.frame(
    report_id                  = make_report_id(ORCID, SSJ_DOI, citing_doi),
    submitted_at               = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    reporter_role              = ROLE,
    reporter_orcid             = ORCID,
    own_work_doi               = SSJ_DOI,
    citing_work_doi            = citing_doi,
    target_author_last_names   = T_AUTHORS,
    target_lead_author         = T_LEAD,
    target_author_key          = T_KEY,
    target_publication_year    = T_YEAR,
    target_title_clue          = T_TITLE_CLUE,
    target_title_key           = T_TITLE_KEY,
    mistake_codes              = codes,
    mistake_titles             = titles,
    quoted_or_paraphrased_text = quote,
    why_incorrect              = why,
    stringsAsFactors = FALSE
  )
}

entries <- list(

  # 1. Godwin et al.
  row(
    "10.1037/xap0000521", "M2", "Non-existent finding",
    "Following recommendations regarding transparent reporting of sample size selections (Lakens, 2022), we recruited as large a sample size as possible given the financial resources and time that we had available for this project. We did, however, conduct a sensitivity power analysis using R once the experiments were complete. [...] Unless otherwise stated in the Results section, our sample size was sufficient to detect all significant effects with 0.8 power.",
    "Lakens (2022) explicitly warns against the claim that 'our sample size was sufficient to detect significant effects' — this reverses the logic of a sensitivity analysis, which should report the smallest detectable effect, not claim sufficiency based on which effects reached significance. The citing paper does precisely what the paper cautions against."
  ),

  # 2. Glavac et al.
  row(
    "10.3389/fpsyg.2024.1440013", "M2", "Non-existent finding",
    "The simulations showed that power levels of the model were adequate based on the conventional cut-off criteria (Lakens, 2022).",
    "Lakens (2022) does not discuss or endorse conventional cut-off criteria for evaluating power levels. The paper explicitly cautions against using generic power thresholds (e.g., 80%) as automatic evidence of adequacy without substantive justification."
  ),

  # 3. Dong et al.
  row(
    "10.1080/1612197X.2024.2435020", "M2", "Non-existent finding",
    "The sample size was constrained by resources and availability of athletes, most of whom play abroad in professional or collegiate leagues (Lakens, 2022).",
    "Lakens (2022) acknowledges resource constraints as one factor but explicitly states this is insufficient on its own: researchers should follow up with a sensitivity analysis showing the smallest detectable effect. Citing resource constraints alone implies the paper endorses this as a complete justification, which it does not."
  ),

  # 4. Borelli and Pesciarelli
  row(
    "10.3389/fpsyg.2024.1474945", "M2", "Non-existent finding",
    "The sample size was determined using a heuristic approach, based on typical participant numbers commonly employed in studies within this field (Lakens, 2022).",
    "Lakens (2022) explicitly states that when using a heuristic based on typical sample sizes in the field, one must reproduce the sample size justification from the study being followed. The citing paper does not do this, misrepresenting the paper as endorsing a bare field-norm heuristic without the required follow-up justification."
  ),

  # 5. Smith et al.
  row(
    "10.1002/casp.70045", "M2", "Non-existent finding",
    "Sample size was determined based on available funding (Lakens 2022)",
    "Lakens (2022) does not endorse available funding as a standalone sample size justification. The paper requires researchers to additionally explain why the resulting sample is still informative — for example by means of a sensitivity power analysis. The citation implies funding constraints alone are a complete justification."
  ),

  # 6. Woodford et al. (first paper)
  row(
    "10.1007/s41252-024-00403-3", "M2", "Non-existent finding",
    "Given the lack of prior research into the treatment of sleep disturbance with this population, constraints of time and financial support, the disruptions caused by COVID-19, and that by definition RGNC are rare, this N (26) is considered sufficient for research to be informative (Lakens, 2022).",
    "Lakens (2022) does not describe a procedure by which listing contextual constraints and population rarity is sufficient to declare a sample 'informative.' The paper requires a concrete informational justification, such as a sensitivity analysis or precision estimate, not a narrative of barriers."
  ),

  # 7. Ibrahim et al.
  row(
    "10.1002/csr.70424", "M2", "Non-existent finding",
    "As Lakens (2022) emphasizes, predictive modeling does not adhere to rigid sample size requirements; our dataset falls well within empirically acceptable ranges for the ML algorithms employed.",
    "Lakens (2022) makes no claims about predictive modeling or machine learning sample size requirements. The paper is exclusively about sample size justification for hypothesis testing in behavioral and social sciences. This is a complete fabrication of the paper's content."
  ),

  # 8. Ramos-Castaneda et al.
  row(
    "10.1111/nhs.70126", "M2", "Non-existent finding",
    "As the designed intervention had not been previously evaluated, the expected effect size was unknown. In addition, the studies published to date that have designed educational interventions for FA have different educational programs and outcomes. Therefore, the required sample size could not be calculated (Lakens 2022).",
    "Lakens (2022) does not state that sample size cannot be calculated when effect size is unknown. The paper explicitly recommends using a Smallest Effect Size of Interest (SESOI) when prior data is unavailable. The claim that calculation was impossible misrepresents the paper's recommendations."
  ),

  # 9. Zhou et al. (dementia caregivers)
  row(
    "10.1007/s12062-025-09514-8", "M10", "Non-contextualized citation",
    "While high post-hoc power estimates may raise concerns, they are justifiable in the presence of large effect sizes, excellent model fit (e.g., RMSEA = 0.026, CFI = 0.998, TLI = 0.996), and stable parameter estimates (Maxwell, 2004; Lakens, 2022).",
    "Lakens (2022) does not discuss conditions under which high post-hoc power estimates are justifiable based on model fit indices or parameter stability. No specific content of the paper is identifiably connected to the citing claim."
  ),

  # 10. Hassanain et al.
  row(
    "10.1108/SASBE-10-2023-0318", "M2", "Non-existent finding",
    "Furthermore, a sample size of 30 has become a widely accepted guideline in many research fields, particularly in social sciences and domains that utilize questionnaire surveys (Lakens, 2022).",
    "Lakens (2022) nowhere endorses a sample size of 30 as a widely accepted guideline. The paper explicitly argues against arbitrary sample size rules of thumb. Attributing this claim to Lakens (2022) is a complete fabrication of the paper's content."
  ),

  # 11. Zaleskiewicz et al.
  row(
    "10.3389/fnhum.2023.1207364", "M2", "Non-existent finding",
    "Since the sample size was restricted mainly by our financial resources and time (Lakens, 2022), we sent an email to the pool of 66 participants who completed the screening study, informing them that the first 31 participants who confirmed their interest in participating in the fMRI study would be invited to the neuroimaging laboratory.",
    "Lakens (2022) acknowledges resource constraints as a starting point but explicitly requires a follow-up justification showing why the resulting sample is still informative (e.g., via sensitivity analysis). Citing financial constraints without any follow-up misrepresents the paper's recommendations as endorsing a bare constraint-based justification."
  ),

  # 12. Woodford et al. (second paper)
  row(
    "10.1007/s41252-024-00399-w", "M2", "Non-existent finding",
    "although the inclusion of a range of RGNC provided an adequate sample size for formal quantitative evaluation (Lakens, 2022)",
    "Lakens (2022) does not define any criterion by which including a range of conditions provides an adequate sample size for formal quantitative evaluation. No substantive justification is offered, contrary to the paper's explicit recommendations."
  ),

  # 13. Ketelings et al.
  row(
    "10.1016/j.appet.2025.107965", "M2", "Non-existent finding",
    "A correlation of .4 among repeated measures was assumed for the sample size calculation (Lakens, 2022).",
    "Lakens (2022) does not recommend or specify a correlation of .4 as an assumption for repeated-measures power calculations. Attributing this specific numerical assumption to the paper is incorrect."
  ),

  # 14. Ranjan et al.
  row(
    "10.34105/j.kmel.2024.16.037", "M2", "Non-existent finding",
    "The final number of valid sample responses collected was 401, considered apt for statistical analyses (Al-Adwan et al., 2021; Gravetter & Forzano, 2011; Lakens, 2022).",
    "Lakens (2022) does not endorse any particular number of responses as 'apt for statistical analyses.' The paper argues explicitly against using arbitrary sample size benchmarks without justification. This appears to be low-quality or AI-assisted attribution."
  ),

  # 15. Nirghin et al.
  row(
    "10.4102/aveh.v84i1.944", "M10", "Non-contextualized citation",
    "With the assistance of a statistician and using the Powering Sample Size statistical software, a sample size of 85 was calculated based on an effect size of 0.5, a significance level of 0.05 and 80% power.",
    "The power analysis uses Cohen's conventional effect size benchmark (d = 0.5) without justification, which Lakens (2022) explicitly cautions against. The paper's specific recommendations — justifying the chosen effect size and power level in context — are not followed. The citation provides no evidence the paper's guidance was applied."
  ),

  # 16. Sun et al.
  row(
    "10.17583/ijep.12938", "M10", "Non-contextualized citation",
    "Purposive sampling was applied in the current research. This sampling methods involved deliberate selection to achieve representativeness of the settings and individuals (Patton, 2015), allowing for the effective collection of data from participants and the acquisition of valuable new information (Lakens, 2022).",
    "Lakens (2022) is a paper on quantitative sample size justification for hypothesis testing. It does not discuss purposive sampling, qualitative sampling strategies, or the claim that deliberate selection enables 'acquisition of valuable new information.' The citation is entirely unrelated to the paper's content."
  ),

  # 17. Mayiwar et al.
  row(
    "10.1002/bdm.2378", "M2", "Non-existent finding",
    "Our sample size was constrained by limited time and financial resources as this was part of a thesis project (Lakens 2022). These constraints led to a substantially smaller sample size than the preregistered N = 400.",
    "Lakens (2022) acknowledges resource constraints but explicitly requires a sensitivity analysis to determine the smallest detectable effect with the obtained sample. No such follow-up is conducted, misrepresenting the paper as endorsing a bare resource constraint justification."
  ),

  # 18. Amanova et al.
  row(
    "10.1080/2331186X.2025.2532237", "M2", "Non-existent finding",
    "A sample size of 240 students and three instructors was determined to ensure the representativeness and reliability of the obtained results (Althubaiti, Citation2023; Hennink & Kaiser, Citation2022; Lakens, Citation2022).",
    "Lakens (2022) does not provide guidelines for determining sample sizes based on representativeness or reliability criteria. The paper focuses on statistical power and precision for hypothesis testing, not on general claims about what sample size ensures representativeness. The attribution is not supported by the paper's content."
  ),

  # 19. Klankers et al.
  row(
    "10.3390/app14062481", "M2", "Non-existent finding",
    "User studies, while essential, suffer drawbacks for systematic testing. Conducting studies requires a significant number of participants, making them time-consuming and reliant on expert knowledge.",
    "Lakens (2022) does not discuss drawbacks of user studies, claim that studies require expert knowledge, or comment on the number of participants needed for systematic testing. This is a complete fabrication with no identifiable connection to the paper's content."
  ),

  # 20. Elsaid et al.
  row(
    "10.1038/s41598-025-05834-z", "M2", "Non-existent finding",
    "research on sample size heuristics indicates that a sample of approximately 30 observations strikes a balance between feasibility and statistical power, making it a widely accepted benchmark in empirical studies",
    "Lakens (2022) explicitly argues against arbitrary heuristics such as N = 30. The paper nowhere endorses 30 as an acceptable benchmark. This appears to be AI-generated text fabricating content from the paper."
  ),

  # 21. Al-Abdallah et al.
  row(
    "10.1155/2023/2541243", "M2", "Non-existent finding",
    "For such a large population, Cochran's formula was used to estimate the cross-sectional sample size to increase the accuracy of the representation, with the application of a 3% margin of error.",
    "Lakens (2022) is cited as a reference for applying a 3% margin of error in Cochran's cross-sectional sampling formula. The paper contains no discussion of Cochran's formula, cross-sectional population surveys, or specific margin-of-error recommendations. This is a fabricated citation, likely AI-generated."
  ),

  # 22. Ahmad et al.
  row(
    "10.1016/j.sftr.2025.101023", "M2", "Non-existent finding",
    "According to Lakens, a small sample size is easy to manage within lower budgetary conditions; therefore, we used a smaller sample at the present instant.",
    "Lakens (2022) does not state that small sample sizes are easy to manage within lower budgetary conditions. The paper discusses resource constraints as a legitimate but insufficient justification and does not advocate using small samples for budget reasons. This appears to be an AI-generated misattribution."
  ),

  # 23. Raphela et al.
  row(
    "10.3389/fpubh.2025.1526309", "M2", "Non-existent finding",
    "However, a sample size of 100 for a quantitative study has been reported to give good statistical inferences.",
    "Lakens (2022) does not state that a sample size of 100 gives 'good statistical inferences.' The paper explicitly argues against arbitrary threshold-based reasoning about sample adequacy. This is a fabricated claim attributed to the paper."
  )

)

cat("Adding", length(entries), "SSJ miscitation entries...\n\n")

for (e in entries) {
  cat("  ->", e$citing_work_doi, "\n")
  tryCatch(
    append_row_safely(e),
    error = function(err) cat("     ERROR:", conditionMessage(err), "\n")
  )
  Sys.sleep(0.05)
}

final_db <- read_reports_db()
cat("\nFinal row count:", nrow(final_db), "\n")

result <- push_db_to_github(
  final_db,
  paste0("Add ", length(entries), " SSJ (Lakens 2022) miscitation entries from citation analysis")
)
cat("GitHub push result:", result, "\n")
