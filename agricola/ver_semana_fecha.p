DEF VAR v-anio AS INTEGER.
DEF VAR v-semana AS INTEGER.
DEF VAR v-desde AS DATE.
DEF VAR v-hasta AS DATE.

v-anio = 2014.
v-semana = 30.
RUN semana_fecha.p (INPUT v-anio,
                    INPUT v-semana,
                    OUTPUT v-desde,
                    OUTPUT v-hasta).
MESSAGE v-desde v-hasta VIEW-AS ALERT-BOX.
