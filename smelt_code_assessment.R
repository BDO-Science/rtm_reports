# Delta Smelt Abiotic Conditions

## Turbidity
Use CDECRetrieve - SensorDownload.R 
Look at just data from the past week 

- Water temperature, Flow
- 3 day averages 
- Which stations? OBI turbidity, CLC water temp... others? Are plots from SMT page desired?
  
  ### TABLE 10.
  
  - NOAA weather service
- 
  ```{r CDECdata, echo = FALSE}
# Define parameters ----------------------------------
start <- today() - 7
end <- today()

# Download data, bind, write --------------------------------------------
turb_OBI <- cdec_query("OBI", "221", "E", start, end)
turb_OBI_clean <- turb_OBI %>%
  dplyr::bind_rows() %>%
  mutate(date = date(datetime)) %>%
  select(date, station = location_id, turbidity = parameter_value) %>%
  group_by(station, date) %>%
  summarize(meanTurbidity = round(mean(turbidity),2)) %>%
  ungroup()

temp_CLC <- cdec_query("CLC", "146", "H", start, end)
temp_CLC_clean <- temp_CLC %>%
  dplyr::bind_rows() %>%
  mutate(date = date(datetime)) %>%
  select(date, station = location_id, watertemp = parameter_value) %>%
  group_by(station, date) %>%
  summarize(meanWaterTemp = round(mean(watertemp),2)) %>%
  ungroup()
```

```{r, echo = FALSE}
flextable(turb_OBI_clean) %>%
  set_caption("TABLE 10a. Weekly Mean Turbidity (FNU) @ Old River at Bacon Island (OBI)") %>%
  vline()%>%
  hline() %>%
  border_outer()

flextable(temp_CLC_clean) %>%
  set_caption("TABLE 10b. Weekly Mean Water Temperature (°C) @ Clifton Court Forebay (CLC)") %>%
  vline() %>%
  hline() %>%
  border_outer()
```

