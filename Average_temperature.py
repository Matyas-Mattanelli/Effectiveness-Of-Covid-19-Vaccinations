import requests
from bs4 import BeautifulSoup
import numpy as np
import pandas as pd

#Obtaining links for all countries
request_countries=requests.get('https://www.weatherbase.com/weather/countryall.php3')
request_countries.status_code
soup_countries=BeautifulSoup(request_countries.content)
countries=soup_countries.find_all('a',{'class':'redglow'})
countries_href=[i['href'] for i in countries]
countries_href_complete=['https://www.weatherbase.com/'+i for i in countries_href]

#Obtaining links for all cities
requests_for_all_countries=[]
for i in countries_href_complete:
    request=requests.get(i)
    if request.status_code==200:
        requests_for_all_countries.append(requests.get(i))
    else:
        print('error'+' '+i)
country_names=[i.text for i in countries]
cities_links_by_country={}
for i in range(len(country_names)):
    soup=BeautifulSoup(requests_for_all_countries[i].content)
    cities=soup.find_all('a',{'class':'redglow'})
    cities_links_by_country[country_names[i]]=['https://www.weatherbase.com/'+i['href']+'&set=metric' for i in cities]

#Obtaining the data for each city in all countries
cities_requests_by_country={}
for i in range(len(country_names)):
    request_list=[]
    for j in cities_links_by_country[country_names[i]]:
        request=requests.get(j)
        if request.status_code==200:
            request_list.append(request)
        else:
            print('Error'+' '+i+' '+j)
    cities_requests_by_country[country_names[i]]=request_list
cities_data_by_country={}
for i in range(len(country_names)):
    data_list=[]
    for j in cities_requests_by_country[country_names[i]]:
        soup=BeautifulSoup(j.content)
        data_all=soup.find_all('td',{'class':'data'})
        avg_temp=[x.text for x in data_all[1:13]]
        final_vector=[]
        for z in avg_temp:
            try:
                final_vector.append(float(z))
            except:
                final_vector.append(z)
        data_list.append(final_vector)
    cities_data_by_country[country_names[i]]=data_list

#Remove empty vectors
cities_data_by_country_final={}
for i in country_names:
    data_list=cities_data_by_country[i]
    cities_data_by_country_final[i]=[x for x in data_list if x!=[]]

#Remove too high or too low values
cities_data_by_country_final_finally={}
for i in country_names:
    data_list=cities_data_by_country_final[i]
    new_data_list=[]
    for j in data_list:
        vector=np.array(j)
        vector[vector=='---']=np.nan
        vector=vector.astype(float)
        if sum((vector>56.7)|(vector<-89.2))>0:
            pass
        else:
            new_data_list.append(vector)
    cities_data_by_country_final_finally[i]=np.array(new_data_list)

#Computing averages accross cities for each country
monthly_avg_temp={}
for i in country_names:
    country_data=cities_data_by_country_final_finally[i]
    monthly_avg_temp[i]=np.nanmean(country_data,axis=0)

#Creating a data frame
df=pd.DataFrame(monthly_avg_temp)
df=df.T

#Renaming columns
df.columns=['January','February','March','April','May','June','July','August','September','October','November','December']
df['Country']=df.index

#Reshaping the data set
df_final=df.melt(id_vars='Country',var_name='Month',value_name='Average_Temperature')

#Exporting to csv
df_final.to_csv('avg_temp.csv',index=False)