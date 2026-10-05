# Football Spatial Analysis

Διαδραστική εφαρμογή R/Shiny για τη **χωρική ανάλυση αγώνων ποδοσφαίρου** με μεθόδους των Συστημάτων Γεωγραφικών Πληροφοριών (ΣΓΠ), σε ανοιχτά δεδομένα γεγονότων της StatsBomb.

**Η εφαρμογή online:** https://tsourakisp.shinyapps.io/football-spatial-analysis/

Η εφαρμογή αναπτύχθηκε στο πλαίσιο της διπλωματικής εργασίας:

> Παράσχος Τσουράκης, *Χωρική ανάλυση αγώνων ποδοσφαίρου με χρήση Συστημάτων Γεωγραφικών Πληροφοριών*, Διπλωματική εργασία, Σχολή Αγρονόμων και Τοπογράφων Μηχανικών – Μηχανικών Γεωπληροφορικής, Εθνικό Μετσόβιο Πολυτεχνείο, 2026. Επιβλέπουσα: Ελένη Τομαή.

*English summary below.*

## Τι κάνει

Ο χρήστης επιλέγει διοργάνωση, σεζόν, αγώνα, ομάδα και παίκτη από τη διεπαφή, χωρίς να γράψει κώδικα. Τα αποτελέσματα σχεδιάζονται πάνω στο γήπεδο και, όπου έχει νόημα, συνοδεύονται από μετρήσεις.

| Ενότητα | Περιεχόμενο |
|---|---|
| Μεταβιβάσεις παίκτη | Χάρτης ροής μεταβιβάσεων, με διάκριση επιτυχημένων και ανεπιτυχών |
| Χάρτης σουτ | Σουτ ομάδας ή παίκτη, με το αποτέλεσμα και τα αναμενόμενα γκολ (xG) |
| Ενέργειες παίκτη | Χάρτης πυκνότητας των ενεργειών και πίνακας ενεργειών ανά τύπο |
| Δίκτυο μεταβιβάσεων | Δίκτυο ομάδας με μετρικές (κεντρικότητα ενδιαμεσότητας και βαθμού, συχνότερα ζεύγη, προωθητικές μεταβιβάσεις), χρονικό παράθυρο, κατηγορία μήκους, σύγκριση των δύο ομάδων, υπόβαθρο πυκνότητας |
| Γκολ σεζόν | Χάρτης των γκολ μιας ομάδας σε ολόκληρη σεζόν |
| Χωρική ανάλυση | Διάγραμμα Voronoi των θέσεων των παικτών και κυρτό πολύγωνο ενός ή δύο παικτών, με εμβαδόν και κοινή επιφάνεια σε m² |

## Εκτέλεση στον υπολογιστή σας

Χρειάζεται η [R](https://www.r-project.org) (η εφαρμογή αναπτύχθηκε με την έκδοση 4.5.2) και, προαιρετικά, το [RStudio](https://posit.co/products/open-source/rstudio).

1. Εγκατάσταση των πακέτων (μία φορά):

   ```r
   install.packages(c("shiny", "shinydashboard", "tidyverse", "ggsoccer", "igraph", "sf", "remotes"))
   remotes::install_github("hudl/StatsBombR")
   ```

2. Εκτέλεση απευθείας από το αποθετήριο:

   ```r
   shiny::runGitHub("football-spatial-analysis", "ParaschosTs")
   ```

   ή, αφού κατεβάσετε το `app.R`, από τον φάκελο στον οποίο βρίσκεται:

   ```r
   shiny::runApp("app.R")
   ```

Εκδόσεις των πακέτων με τις οποίες αναπτύχθηκε και ελέγχθηκε η εφαρμογή: shiny 1.13.0, shinydashboard 0.7.3, tidyverse 2.0.0 (dplyr 1.2.0, ggplot2 4.0.2), StatsBombR 0.1.0, ggsoccer 0.2.0, igraph 2.3.1, sf 1.1.1.

## Δεδομένα

Τα δεδομένα προέρχονται από τη βάση [StatsBomb Open Data](https://github.com/hudl/open-data). Η εφαρμογή τα ανακτά από το δημόσιο αποθετήριο της StatsBomb κατά την εκτέλεση, με το πακέτο [StatsBombR](https://github.com/hudl/StatsBombR), και δεν τα αποθηκεύει. Το αποθετήριο αυτό δεν περιέχει δεδομένα.

Η χρήση των δεδομένων διέπεται από τους όρους της StatsBomb. Αν δημοσιεύσετε αναλύσεις οι οποίες βασίζονται σε αυτά, η StatsBomb ζητά να αναφέρεται ως πηγή των δεδομένων και να χρησιμοποιείται το λογότυπό της.

## Ο κώδικας

Όλη η εφαρμογή βρίσκεται στο αρχείο `app.R`, το οποίο οργανώνεται σε τέσσερα τμήματα:

1. πακέτα και σταθερές,
2. βοηθητικές συναρτήσεις, οι οποίες εκτελούν τους υπολογισμούς και παράγουν τα γραφήματα κάθε ανάλυσης,
3. διεπαφή (`ui`),
4. εξυπηρετητής (`server`).

Η σχεδίαση, οι μέθοδοι και οι περιορισμοί της εφαρμογής περιγράφονται αναλυτικά στο κείμενο της διπλωματικής εργασίας.

## Άδεια χρήσης

Ο κώδικας διατίθεται με την άδεια MIT (αρχείο `LICENSE`). Η άδεια αφορά μόνο τον κώδικα και όχι τα δεδομένα της StatsBomb.

---

## English summary

**Football Spatial Analysis** is an interactive R/Shiny application for the spatial analysis of football matches with GIS methods, built on StatsBomb open event data. It was developed for a diploma thesis at the School of Rural, Surveying and Geoinformatics Engineering, National Technical University of Athens (2026). The interface is in Greek.

The app offers pass flow maps, shot maps with expected goals (xG), kernel density maps of player actions, pass networks with centrality metrics, season goal maps, Voronoi diagrams and convex hulls of player actions with areas in m².

- Live app: https://tsourakisp.shinyapps.io/football-spatial-analysis/
- Run locally: install the packages listed above, then `shiny::runGitHub("football-spatial-analysis", "ParaschosTs")`.
- Data: retrieved at run time from [StatsBomb Open Data](https://github.com/hudl/open-data); no data are stored in this repository. Please credit StatsBomb as the data source.
- Licence: MIT for the code.
