#!/usr/bin/node

var R = require('ramda');
require('sylvester');

var building;

process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', function (building) {
	process.stdout.write(calculate(building) + '\n');
});
//vector operations
V = {
	x: function (a, v) {
		return [a * v[0], a * v[1]];
	},
	add: function (v, w) {
		return [v[0] + w[0], v[1] + w[1]];
	}
}

var addOrientation = R.curry( function (o, el) {
	var orientation = function (v, o) {
		return o + 180 / Math.PI * $V(v).angleFrom($V([0, 1]) );
	}
	el.orientation  = orientation(el.geom, o);
	return el;
} );

var getDiafani = function (el, i) {
	var d = el.diafani;
	return d && R.map(function (d) {
		d.index = i;
		return d;
	})(d);
}
var findPoint = function (startPoint, edges) {
	return R.reduce(function (a, b) {
		return V.add(a, b);
	}, startPoint) (edges);
}
var addEnd = function (b, edges) {
	var balconyStart = findPoint(V.x(b.start.position, edges[b.start.edge]), R.take(b.start.index - 1, edges) );
	console.log(balconyStart);
	var endPoint = findPoint(balconyStart, b.geom);
	return endPoint; 
}

var calculate = function (building) {
	var b = JSON.parse(building);
	var edges = R.pluck('geom', b.edges);

	var d = R.map(R.compose(addOrientation(b.orientation)) ) (b.edges); 
	
	var balcony = addEnd(b.balconies[0], edges);
	
	console.log(balcony);
	
	var diafani = R.compose(R.reject(R.isNil), R.addIndex(R.map)(getDiafani))(d);
	
	return "";//JSON.stringify(b.edges, null, 2);
}
