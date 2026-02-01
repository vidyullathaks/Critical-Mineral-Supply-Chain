# Sustainability in Critical Mineral Pathways  
### Mapping Uncertainty to Strengthen Critical Mineral Supply Chains

This capstone project evaluates the sustainability, cost, and resilience of the **global cobalt supply chain** used in electric vehicle (EV) batteries.  
Using an optimization-based supply chain model, the project quantifies **transport costs, carbon emissions, and geopolitical risk** under multiple future scenarios.

The work focuses on identifying **trade-offs between cost efficiency, emissions reduction, and supply security** in the context of rising EV demand and concentrated critical mineral supply chains.

---

## 🔍 Problem Context

Critical minerals like **cobalt** are essential to clean energy technologies but face:
- Extreme geographic concentration (≈70% mining in the DRC, ≈80% refining in China)
- Long, carbon-intensive transport routes
- High vulnerability to geopolitical and policy disruptions

As EV adoption accelerates, these risks become increasingly material for governments and industry.

---

## 🎯 Project Objectives

- Map the global cobalt supply chain from mining to battery manufacturing
- Quantify **transport costs and CO₂ emissions** at each stage
- Identify **single points of failure** and resilience risks
- Evaluate how alternative supply chain strategies affect sustainability outcomes

---

## 🧠 Methodology

- **Cradle-to-gate supply chain modeling** (mine → refinery → battery plant)
- **Linear programming optimization** to minimize:
  - Transport cost
  - Transport emissions
- **Scenario analysis** simulating:
  - Trade barriers and tariffs
  - Domestic and diversified refining
  - Clean (low-carbon) transport technologies
- **Monte Carlo sensitivity analysis** (10,000+ runs) to capture uncertainty
- **R-based modeling and visualization**, including an interactive **Shiny dashboard**

---

## 📊 Key Scenarios Analyzed

- **Baseline (Current State)** – Cost-optimal global routing
- **Tariff on Chinese Refining** – Trade disruption simulation
- **U.S. Domestic Refining** – Onshoring midstream processing
- **Diversified Refining** – Reduced dependence on any single country
- **Clean Transport Scenario** – Electrified and low-carbon freight

---

## 📈 Key Findings

- Transporting cobalt for one EV battery emits **~50–100 kg CO₂**, largely from long-distance shipping
- The current supply chain is **near cost-optimal but highly fragile**
- Diversifying refining locations improves resilience and can reduce emissions, but increases cost
- **Decarbonizing transport** yields the largest emissions reduction without changing supply routes
- Battery chemistry (cobalt content) is the **largest sensitivity driver**, outweighing many policy changes

---

## 🛠️ Tools & Technologies

- **R** (optimization, simulation, visualization)
- Linear programming (cost & emissions minimization)
- Monte Carlo simulation
- R Shiny (interactive dashboard)
- Data sources: USGS, World Bank (WITS), IPCC, IEA

---

## 📁 Repository Structure
- /R
- /code          # Optimization & analysis scripts
- /data          # Input datasets
- /visualization # Scenario & sensitivity plots
- /docs            # Project report and supporting documents
---

## 👥 Team

- Sachi Nandurkar  
- **Vidyullatha K. Sathishrao**  
- Vedanth Hegde  
- Mobolaji (Ife) Olaniyan  
- Massimo Mariani  

Course: **EMSE 6099 – Problems in Engineering Management and Systems Engineering**  
George Washington University

---

## 📄 Full Report

📘 *Sustainability in Critical Mineral Pathways: Mapping Uncertainty to Strengthen Critical Mineral Supply Chains*  
(See `/docs/Sustainability in Critical Mineral Pathways.pdf`)

---

## 💡 Why This Project Matters

This work provides a **decision-support framework** for policymakers, manufacturers, and supply chain strategists navigating:
- Clean energy transitions
- Supply chain risk
- Climate impact reduction

The modeling approach is adaptable to **other critical minerals** (lithium, nickel, rare earths) facing similar challenges.

---

## 📬 Contact

For questions or collaboration:
- GitHub: [@vidyullathaks](https://github.com/vidyullathaks)
