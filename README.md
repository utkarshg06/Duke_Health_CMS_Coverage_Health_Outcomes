# Capstone Project: Medicaid Expansion & Health Outcomes

## 📌 Project Title
**Analyzing the Impact: How Expanding CMS Coverage Affects Health Outcomes Across the U.S.**

## 🧠 Summary
This capstone project investigates the causal effects of Medicaid expansion under the Affordable Care Act (ACA) on health outcomes across U.S. states. Using a natural experiment framework and panel data spanning 2010–2019, we analyze trends and estimate treatment effects on communicable and non-communicable diseases (CDs and NCDs) through robust statistical models including Difference-in-Differences (DiD) and Fixed Effects (FE).

## 🎯 Research Motivation
- **Policy Relevance**: Medicaid expansion timing varied by state, enabling causal analysis.
- **Health Equity**: Focused on outcomes for vulnerable populations.
- **Economic & Public Health Value**: Understanding how policy translates to changes in mortality, DALYs, and disease incidence/prevalence.

## 🗂️ Dataset
- **Source**: IHME Global Burden of Disease dataset.
- **Time Frame**: 2010–2019.
- **Metrics**: Deaths, DALYs, Incidence, Prevalence.
- **Stratification**: By state, disease, gender, age group, and Medicaid expansion status.

## 🔍 Methodology
- **Quasi-Experimental Design**: Exploits staggered expansion across states.
- **Modeling Techniques**:
  - Difference-in-Differences (DiD)
  - Fixed Effects (State & Year level)
- **Key Covariates**: Age, sex, state fixed effects, time fixed effects.
- **Treatment Groups**: Early, Mid, Late, and Never expansion states.

## 📊 Key Findings

### Communicable Diseases
- **Early Expansion States**:
  - ↓ HIV/AIDS prevalence and DALYs.
  - ↓ Acute hepatitis and neonatal/maternal sepsis deaths.
- **Mid Expansion States**:
  - Mixed outcomes; some increases in TB, STIs.
- **Late Expansion States**:
  - Limited significant effects due to short post-period.

### Non-Communicable Diseases
- **Post-2014**:
  - ↓ Deaths and DALYs in CKD, diabetes, substance use (especially in early states).
  - ↑ Prevalence and incidence reflects improved screening, not worsening disease.

## 📈 Visualizations
- Trend lines for each metric (2010–2019).
- Heatmaps by age and disease.
- Top 10 conditions by burden.
- Interactive [Tableau Dashboard](https://public.tableau.com/app/profile/jianjun.lei2308/viz/Capstone_17451914903710/Dashboard2)

## ⚠️ Limitations
- Potential violation of parallel trends.
- Endogeneity between prevalence and mortality.
- Limited within-state heterogeneity and confounder resolution.

## 🔭 Future Directions
- Incorporate synthetic control or causal forests.
- Extend analysis with post-2020 data (e.g., COVID-era shifts).
- Explore subpopulation impacts (e.g., rural vs. urban, race).

## 👥 Team Members
- **Communicable Disease**: Tess Li, Jojo Dong, Jianjun Lei, Jingyang Zhou, Jingyi Shao  
- **Non-Communicable Disease**: Richard Xie, Danielle Dawazhuoma, Shrishti Agarwal, Utkarsh Gupta, Lizzie Wang

## 📚 References
- [Commonwealth Fund 2023](https://www.commonwealthfund.org)
- [KFF Reports](https://www.kff.org)
- [PubMed Literature](https://pubmed.ncbi.nlm.nih.gov/)
- [NBER Working Papers](https://www.nber.org/)
- See full reference list in presentation Appendix.

---
