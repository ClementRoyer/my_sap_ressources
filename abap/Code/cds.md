# CDS 


## Dats

```
define view zxxxxx
  with parameters
    _date : budat,
  as select from xxxxxxxx
{

  $parameters.start_date                                                                                                                    as param_date,
  concat( SUBSTRING( DATS_ADD_MONTHS( $parameters._date, - 1, 'UNCHANGED' ), 1, 4 ), '0101' )                                               as calc_firstdate_year_prev_month,
  DATS_ADD_DAYS( cast(CONCAT(SUBSTRING( DATS_ADD_MONTHS($parameters._date, 1, 'UNCHANGED'), 1, 6 ), '01' ) as abap.dats), -1, 'UNCHANGED' ) as calc_last_day_of_curr_month
  DATS_ADD_DAYS( cast(CONCAT(SUBSTRING( $parameters._date, 1, 6 ), '01' ) as abap.dats), -1, 'UNCHANGED' )                                  as calc_last_day_of_last_month,

}

where
  and xxx.budat  between concat( SUBSTRING( DATS_ADD_MONTHS( $parameters._date, - 1, 'UNCHANGED' ), 1, 4 ), '0101' ) and DATS_ADD_DAYS( cast(CONCAT(SUBSTRING( DATS_ADD_MONTHS($parameters._date, 1, 'UNCHANGED'), 1, 6 ), '01' ) as abap.dats), -1, 'UNCHANGED' )

```
