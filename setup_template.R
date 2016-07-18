require(ckanr)
CKAN="http://internal-data.dpaw.wa.gov.au/"
APIKEY="my-api-key"
ckanr::ckanr_setup(url=CKAN, key=APIKEY)

# CKAN resource IDS
MDB_RID="9e81c360-cfe0-4fc0-87f7-d3b64ec57be4"
ETL_RID="1df672e2-4e42-4145-8be2-40c02a8f9319"
SITES_RID="efdced11-ced6-42aa-92a9-259bc720ae28"
SITES_CSV_RID="dd3a8cff-2488-4ea0-990f-103984197a4c"
SURVEYS_RID="dbd6d236-a478-42f3-8731-5bc7dc12666e"
CRAWL_RID="b8781eff-d807-4096-8ffe-0a0a8af7abcf"
NEST_RID="477ada0f-9116-4f9a-b282-2e5c4fe803d6"
NEW_NEST_RID="24e90f53-e696-44cb-b547-4e800d819137"
FALSE_CRAWL_RID="1c7ec216-bc28-4181-bf8a-2d1528f61717"


# MS SQL Server for tagging db
SRV='db-server-name'
DB='db-name'
UN='db-username'
PW='db-password'
TAG_CON=paste0('driver={SQL Server};server=', SRV, ';database=', DB,';username=', UN, ';password=', PW)
