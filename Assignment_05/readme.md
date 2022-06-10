# Excersice 1

Es fällt auf, dass für a = 0 die gesamte Population den Wert b annimmt, was der Berechnung auch auch Sinn macht. Dadurch gibt es keine Variabilität mehr. Die Individuen hängen vollstädnig von b ab.
Für a = 1 und b = 0 handelt es sich um die unveränderten Fitnesswerte.
Durch b werden die Ergebnisse direkt beeinflusst und lassen sich direkt (1 zu 1) verschieben.
Grundsätzlich läst sich durch die Ergebnisse erkennen, dass b einen Minimalwert darstellt, aller Individuen nach der Skalierung darstellt.
Der Faktor a beeinflusst die Skalierung ebenfalls, allerdings werden durch in die "Proportionen" beibehalten. Der Wert 0.2 wird z.B. für a=4 zu 0.8 und der Wert 0.9 zu 3.6. Es kann also wie davor der Wert 0.2 mit dem Wert 0.9 verglichen wurde jetzt der Wert 0.8 mit dem Wert 3.6 verglichen werden.
Bei dem Faktor b ist dies nicht so einfach, da für b=10 der Wert 10.2 mit 10.9 verglichen wird und der Unterschied der einzelnen Fitnesswerte für ein großes b weniger Gewicht haben kann.

# Exersice 3
Man erkennt bei Vergleich der Single - und der Whole Arithmetic Recombination schnell, dass die Whole Arithmetic Recombination die Punkt wesentlich schneller näher zusammen bringt und somit die convexe Hülle aller Datenpunkte verkleinert. Dies macht auch Sinn, da nicht immer nur ein Datenpunkt, sonder gleich alle Datenpunkte betrachtet werden.
Bei der Whole Arithmetic Recombination fällt beim Vergleich der Grafik für alpha auf, dass mit festem alpha=0.5 (es werden immer die zwei gleiche Individuen erzeugt) die Punkte sehr schnell zusammen rücken und die convexe Hülle wesentlich schneller kleiner wird als mit einem random alpha. Es fällt allerdings natürlich ebenfalls auf, dass dadruch, dass immer zwei gleiche Individuen erzeugt werden die Population nur halb so viele verschiedene Individuen besitzt. Der Whole Arithmetic Recombination Algorithmus erzielt im Vergleich mit beiden Varianten des Single Arithmetic Recombination Algorithmus wesentlich schneller "gute" Ergebnisse.