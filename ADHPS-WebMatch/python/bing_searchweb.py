# -*- coding: utf-8 -*-
# Reference: http://www.guguncube.com/2771/python-using-the-bing-search-api

"""
This version: January 2019
Citation: 
Autor, David, David Dorn, Gordon H. Hanson, Gary Pisano, and Pian Shu, 􏰄Foreign Competition and Domestic Innovation: Evidence from U.S. Patents,􏰅 NBER Working Paper No. 22879, December 2016.
Please update the API key before running the script.
"""

import urllib
import urllib2
import json

def get_all_links(query):
    #search_type: Web, Image, News, Video
    key = '[ENTER API KEY]'
    query = urllib.quote(query)
    # create credential for authentication
    user_agent = 'Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; Trident/4.0; FDM; .NET CLR 2.0.50727; InfoPath.2; .NET CLR 1.1.4322)'
    credentials = (':%s' % key).encode('base64')[:-1]
    auth = 'Basic %s' % credentials
    url = 'https://api.datamarket.azure.com/Data.ashx/Bing/SearchWeb/Web?Query=%27'+query+'%27&$top=5&$format=json'
    
    try:
        request = urllib2.Request(url)
        request.add_header('Authorization', auth)
        request.add_header('User-Agent', user_agent)
        request_opener = urllib2.build_opener()
        response = request_opener.open(request) 
        response_data = response.read()
        json_result = json.loads(response_data)
        result_list = json_result['d']['results']
        links = []
        for result in result_list:
            if u'Url' in result.viewkeys():
                 links.append(result[u'Url'].encode('utf-8'))
            if u'Title' in result.viewkeys():
                links.append(result[u'Title'].encode('utf-8'))
            if u'Description' in result.viewkeys():
                links.append(result[u'Description'].encode('utf-8'))
    # import pdb; pdb.set_trace()

    except urllib2.HTTPError as e:
        links=["ERROR", e,"","","","","","","","","","","","",""]

    return links