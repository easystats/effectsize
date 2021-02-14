# print

    Code
      print(effectsize(t.test(1:10, y = c(7:20))))
    Output
      Cohen's d |         95% CI
      --------------------------
      -2.19     | [-3.28, -1.19]
      
      - Estimated using un-pooled SD.

---

    Code
      print(effectsize(chisq.test(as.table(rbind(c(762, 327, 468), c(484, 239, 477),
      c(484, 239, 477))))))
    Output
      Cramer's V |       95% CI
      -------------------------
      0.07       | [0.05, 0.09]

---

    Code
      print(effectsize(oneway.test(extra ~ group, data = sleep, var.equal = TRUE)))
    Output
      Eta2 |       90% CI
      -------------------
      0.16 | [0.00, 0.41]

---

    Code
      print(equivalence_test(es, range = 0.15))
    Output
      # Test for Practical Equivalence
       	ROPE: [0.00 0.15]
      
      Parameter              | Eta2 (partial) |       90% CI |        H0
      ------------------------------------------------------------------
      factor(am)             |           0.63 | [0.42, 0.75] |  Rejected
      factor(cyl)            |           0.66 | [0.45, 0.77] |  Rejected
      factor(am):factor(cyl) |           0.10 | [0.00, 0.27] | Undecided

---

    Code
      print(rules(c(small = 0.2, medium = 0.5), name = "Cohen's Rules"))
    Output
      # Reference values (Cohen's Rules)
      
      Label | small | medium
      ----------------------
      Value |  0.20 |   0.50

