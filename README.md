

- add intro

## Before using `fetchRawData()`

### Fill out metadata in the AGOL database you want to import

For each of the tables in the database you must fill out the Entity (table) and Attribute metadata. This is found in each table under `Metadata -> All metadata -> Attributes -> Data Dictionary`. Here you must fill out the information for the table and each of the attributes in the table. See example below for help. Attribute information can be found on the `Fields` tab of the `Data` section of each table. The class is the the EML type and can either be string, integer, decimal, or dateTime. For integer and decimal classes units should be provided.

- add an example of filled out metadata
- add how to trouble shoot by looking at query

### Set up headless account

To use the AGOL API you should have a headless account for scripting that has permission to access the AGOL database. You might want to save your headless account password using the `keyring` package to avoid having to hard code your password.

- is a headless account optional??

