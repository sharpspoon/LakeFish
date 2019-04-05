#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# =============================================================================
# Created By  : Brad W Vick
# Created Date: 11/7/2018
# =============================================================================
"""
    The Module is designed to be the driver for downloading NLDAS GRB files from
    the web.  It will then aggregate the GRB files into daily netCDF files.

    It will do this in a loop for each day in a given date range
    (startDate, endDate).

    The module uses wget to download the files.

    The website that holds these GRB files has indexing so we can download
    all files in a single directory at once

    The files are located at https://hydro1.gesdisc.eosdis.nasa.gov/data/NLDAS/NLDAS_FORA0125_H.002/YYYY/DDD
    The naming convention for the files is NLDAS_FORA0125_H.AYYYYMMDD.HHHH.002.grb
    Where
        YYYYY = year
        MM = Month
        DDD = Julian day of year
        HHHH = 1 hour average interval (0000, 0100, 0200, ....2300)

 """

#files required for wget
#.netrc - need to update with username/password to NLDAS data website
#.usr_cookies

