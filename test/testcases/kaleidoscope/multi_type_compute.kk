using printf(string ...): int;

printf("%f\n", 42. * 42);
printf("%f\n", 42 * 42.);
printf("%d\n", (1 == 1) * 42);
printf("%d\n", 42 * (1 == 1));
printf("%f\n", (1 == 1) * 42.);
printf("%f\n", 42. * (1 == 1));
printf("%f\n", (1 == 1) * 42 * 42.);
printf("%f\n", (1 == 1) * 42. * 42);
printf("%f\n", 42 * (1 == 1) * 42.);
printf("%f\n", 42. * (1 == 1) * 42);
printf("%f\n", 42. * 42 * (1 == 1));
printf("%f\n", 42 * 42. * (1 == 1));
