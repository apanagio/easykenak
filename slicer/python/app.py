#!/usr/bin/python3

from sys import stdin
import json

def pretty_print(json_object):
    print(json.dumps(json_object, sort_keys=True, indent=4, separators=(',', ': ')))

buildingString = stdin.read()
building = json.loads(buildingString)

pretty_print (building["edges"])

