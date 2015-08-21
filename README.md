# Z Instant Comprehensive ABAP - Data Dictionary Table Comparison Tool

This program compares data for an ABAP Data Dictionary table from
two different systems (eg: DEV and PRD) and exports a DIFF.
How to use it:

- First, you'll need to extract data from your systems by using
  FM/method GUI_DOWNLOAD with WRITE_FIELD_SEPARATOR = 'X'.
- Name the files as <TABLE_NAME>.TXT. (eg: for MARA, use MARA.TXT)
- Run the program and fill in the parameters.
- Open the DIFF file in Excel and have fun! :)
