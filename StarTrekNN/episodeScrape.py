# -*- coding: utf-8 -*-
import csv
import requests
import json
from bs4 import BeautifulSoup
import time
import io

filename = open("allEpisodes.csv", "r")
f=filename.readlines()
filename.close()
	
for line in f:
	url = "http://memory-alpha.wikia.com/wiki/" + line.strip()
	print(url.replace('"',''))
	r = requests.get(url.replace('"',''))
		
	soup = BeautifulSoup(r.content.decode('utf-8', 'ignore').encode('utf-8', 'ignore').decode(), "html.parser")
	#soup = BeautifulSoup(r.content.decode('utf-8', 'ignore'), "html.parser")
	
	[s.extract() for s in soup('p', {"class": "caption"})]

	#selects all a tags that have title or href in them and deletes them. Unwraps a tags.
	for a in soup.find_all('a'):
		del a['href']
		del a['title']
		a.unwrap()
		
	#removes angle bracket italics tags
	for a in soup.find_all("i"):
		a.unwrap()
		
	p_tag = soup.find_all("p")	

	with io.open ("alloutput.txt", "a", encoding="utf-8") as myfile:
		for item in (p_tag):		
			myfile.write(item.text)
	time.sleep(1)