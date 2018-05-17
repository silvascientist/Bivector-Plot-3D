(* ::Package:: *)

BeginPackage["Bivectors`"]

Bivector::usage = "Bivector[v1,v2,pt] creates a graphics object which depicts the bivector v1\[TensorWedge]v2, based at point pt"
BivectorPlot3D::usage = "BivectorPlot3D[T,{x,\!\(\*SubscriptBox[\(x\), \(min\)]\),\!\(\*SubscriptBox[\(x\), \(max\)]\)},{y,\!\(\*SubscriptBox[\(y\), \(min\)]\),\!\(\*SubscriptBox[\(y\), \(max\)]\)},{z,\!\(\*SubscriptBox[\(z\), \(min\)]\),\!\(\*SubscriptBox[\(z\), \(max\)]\)}] plots the bivector field T over the specified range"

Begin["`Private`"]
Options[Bivector]={Based->"Mid",ShowPoint->False};
Options[BivectorPlot3D]=Append[Options[Bivector],Scaling->"Normal"];
BivectorPlot3D::options="Incorrect option supplied";
Bivector[v1_,v2_,point_,OptionsPattern[]]:=((*Draws the bivector*)
If[OptionValue[Based]=="Tail",p=point,
If[OptionValue[Based]=="Mid",p=point-(v1+v2)/2,
If[OptionValue[Based]=="Tip",p=point-v1-v2,
Message[BivectorPlot3D::options]]]];{Arrowheads[Medium],Arrow[{p,p+v1}],Arrow[{p,p+v2}],Arrow[BezierCurve[{p+(v1+v2)/4,p+v1,p+v1+v2,p+v1/4+3v2/4}]],Polygon[{p,p+v1,p+v1+v2,p+v2}],If[OptionValue[ShowPoint]==True,{Orange,Point[point]}]});
BivectorPlot3D[T_,xrange_,yrange_,zrange_,opts:OptionsPattern[]]:=(
F=Normal[HodgeDual[T,3]];
span=Orthogonalize@NullSpace[{F}];(*Creates spanning vectors for the blade*)
If[OptionValue[Scaling]=="Normal",
max=Max[Table[Norm[F],xrange,yrange,zrange]];
span=1.5*Norm[F]*span/max,(*Blades should be scaled according to the magnitude of the bivector field*)
If[OptionValue[Scaling]=="Constant",
span=0.75*span,(*Blades should have constant scaling*)
Message[BivectorPlot3D::options]]];
bivecs=Table[
Bivector@@Join[
Quiet[Check[If[F.Cross@@span>0,span,Reverse[span]],{{0,0,0},{0,0,0}}],{Power::infy,Infinity::indet,Det::mindet}],(*Checks whether spanning vectors are in the right orientation; Also checks for singularities in span that may result from NullSpace*)
{{xrange[[1]],yrange[[1]],zrange[[1]]}},Evaluate[FilterRules[{opts}, Options[Bivector]]]],
xrange,yrange,zrange];
Graphics3D[bivecs]);
End[]
EndPackage[]



