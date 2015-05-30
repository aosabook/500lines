#!/usr/bin/env python
# -*- coding: utf-8 -*- #
from __future__ import unicode_literals

AUTHOR = u'Tavish Armstrong'
SITENAME = u'The Performance of Open Source Software'
SITEURL = ''

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = u'en'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None

STATIC_PATHS = [
#    'chrome-images',
#    'ethercalc-images',
#    'infinispan-images',
#    'khmer-images',
#    'memshrink-images',
#    'mobile-perf-images',
#    'pugixml-images',
#    'talos-images',
#    'warp-images',
#    'zotonic-images',
        ]


DEFAULT_PAGINATION = False

THEME = 'posa'

# TODO This turns --- into &mdash;. Why? :(
TYPOGRIFY = True

EXTENSIONS = ('latex',)

# Uncomment following line if you want document-relative URLs when developing
#RELATIVE_URLS = True
