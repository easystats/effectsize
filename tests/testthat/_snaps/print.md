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

