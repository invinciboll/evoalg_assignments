#include <stdio.h>

// m=matrix as "vector", k = Anzahl Zeilen, l = Anzahl Spalten, res = Potiner auf Ergebnis
void sum2(double *m, int *k, int *l, double *res)
{
  for (int i = 0; i < *k; i++)
  {
    for (int j = 0; j < *l; j++)
    {
      // i * length(j) + j ergibt den korespondierenden Eintrag in der Matrix
      *res = *res + m[i * *l + j];
    }
  }
}