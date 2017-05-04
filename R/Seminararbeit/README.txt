Übersicht über die ordnerstruktur:
Ordner:
 - Grafiken: Hier werden automatisch alle Grafiken abgespeichert die in der Seminarbeit und dem Vortrag verwendet werden.
 - RCode Fuchs Original: Der unveränderte Online Appendix des Nächsten Nachbarn Ensembles von Fuchs et al
 - RCode Fuchs Original Update: Der online Appendix des Nächsten Nachbarn Ensembles und zusätzlicher Code von Karen Fuchs
   Anmerkung: Im Rahmen dieser Seminararbeit wurden Fehler in den Files pi_calculation.R und pi_calculation_val.R in den Ordnern 
   RCode Fuchs Original zw. RCode Fuchs Original Update entdeckt und ausgebessert. Daher ist eine Verwendung der aktualisierten 
   Files (siehe .R Files) für weitere Analysen empfehlenswert.

.R-Files:
 - ensemble_weights_plot.R: Generiert die Grafiken im Ausblick zur Auswirkung verschiedener Gewichtung der Semimetriken im Ensemble
 - ferraty_example_plot.R: Erzeugt Grafiken für das Beispiel zum Nonparametric Functional Kernel Estimator (NFKE) auf einem Teil der Berkeley Growth Study
 - ferraty_original.R: Enthält den Code der von Ferraty und Vieu zu ihrem Buch bereitgestellt wird
 - ferraty_test.R: Test des NFKE für verschiedene Semimetriken. Achhtung: Die auf Dynamic Time Warping basierenden Semimetriken werden 
   aufgrund des hohen Rechenaufwands auf mehreren Kernen parallelisiert gerechnet.
 - fuchs_test.R: Test des NNE für verschiedene Semimetriken.
 - growth_extract_tuples.R: Dieses File muss nach fuchs_test.R ausgeführt werden um die Gewichte im Ensemble zu extrahieren und als Grafik abzuspeichern
 - growth_study_data.R: Liest die Daten der Berkeley Growth Study ein und stellt transformiert sie in die benötigten Formate
 - growth_study_plots.R: Erstellung deskriptiver Grafiken zur Berkeley Growth Study
 - MY_NPCD_funcCollection_pene.r: Enthält den Code aus ferraty_original.R um selbstgeschriebene Semimetriken erweitert
 - pi_calculation.R, pi_calculation_val.R, semimetrics_definition.R, semimetrics_definition_val.R: Aktualisierter Code aus dem Online Appendix von Fuchs et al