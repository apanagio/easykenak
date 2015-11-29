#!/usr/bin/node

var R = require('ramda');
require('sylvester');

var building;

process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', function (building) {
	process.stdout.write(calculate(building) + '\n');
});

var calculate = function (building) {
	var b = JSON.parse(building);
	console.log($V([1,1,1]).log());
	return JSON.stringify(b.edges, null, 2);
}
