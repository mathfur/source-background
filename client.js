var margin = {top: 0, right: 0, bottom: 0, left: 0},
    width = (window.innerWidth || documentElement.clientWidth || getElementsByTagName('body')[0].clientWidth) - margin.left - margin.right,
    height = (window.innerHeight || documentElement.clientHeight || getElementsByTagName('body')[0].clientHeight) - margin.top - margin.bottom;

var x = d3.scale.ordinal()
          .rangeRoundBands([0, width]);

var y = d3.scale.ordinal()
          .rangeRoundBands([0, height]);

var svg = d3.select("body")
            .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .attr("transform", "translate(" + margin.left + ", " + margin.top + ")");

var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
var ws = new Socket('ws://localhost:9160/');

ws.onmessage = function(event){
  var json = eval("(" + event.data + ")");
  var nodes = json.nodes;
  var links = json.links;
  var top_line = json.editor_state.top_line;
  var bottom_line = json.editor_state.bottom_line;
  var win_width = json.editor_state.win_width;

  x.domain(d3.range(win_width));
  y.domain(d3.range(top_line, bottom_line));

  svg.selectAll(".label").remove();
  svg.selectAll(".label")
     .data(nodes)
     .enter()
     .append("rect")
     .attr("class", "label")
     .filter(function(d){ return top_line <= d.line && d.line <= bottom_line })
     .attr("title", function(d){ return (d.word || '(none)') + " (" + d.col + ", " + d.line + ")" })
     .style("fill", function(d){
       if(d.count == 1){
         return "#F00";
       }else if(d.count == 2){
         return "#0F0";
       }else if(d.count == 3){
         return "#00F";
       }else{
         return "#aaa";
       }
     })
     .attr("stroke-width", 0)
     .call(set_location);

  svg.selectAll(".edge_mark")
     .data([
         {col: 0,             line: top_line,        length: 1},
         {col: 0,             line: bottom_line - 1, length: 1},
         {col: win_width - 2, line: top_line,        length: 1},
         {col: win_width - 2, line: bottom_line - 1, length: 1}
     ])
     .enter()
     .append("rect")
     .attr("class", "edge_mark")
     .attr("title", "edge")
     .call(set_location)
     .attr("fill", "#000");

};

function set_location(e){
  return e.attr("x", function(d){ return x(d.col) })
          .attr("y", function(d){ return y(d.line) })
          .attr("width", function(d){ return x(d.col + d.length) - x(d.col) })
          .attr("height", function(d){ return y.rangeBand() });
}
