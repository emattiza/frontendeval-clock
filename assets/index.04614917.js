const ve=function(){const n=document.createElement("link").relList;if(n&&n.supports&&n.supports("modulepreload"))return;for(const a of document.querySelectorAll('link[rel="modulepreload"]'))e(a);new MutationObserver(a=>{for(const u of a)if(u.type==="childList")for(const i of u.addedNodes)i.tagName==="LINK"&&i.rel==="modulepreload"&&e(i)}).observe(document,{childList:!0,subtree:!0});function t(a){const u={};return a.integrity&&(u.integrity=a.integrity),a.referrerpolicy&&(u.referrerPolicy=a.referrerpolicy),a.crossorigin==="use-credentials"?u.credentials="include":a.crossorigin==="anonymous"?u.credentials="omit":u.credentials="same-origin",u}function e(a){if(a.ep)return;a.ep=!0;const u=t(a);fetch(a.href,u)}};ve();function z(r,n,t){return t.a=r,t.f=n,t}function f(r){return z(2,r,function(n){return function(t){return r(n,t)}})}function g(r){return z(3,r,function(n){return function(t){return function(e){return r(n,t,e)}}})}function U(r){return z(4,r,function(n){return function(t){return function(e){return function(a){return r(n,t,e,a)}}}})}function tr(r){return z(5,r,function(n){return function(t){return function(e){return function(a){return function(u){return r(n,t,e,a,u)}}}}})}function hr(r){return z(6,r,function(n){return function(t){return function(e){return function(a){return function(u){return function(i){return r(n,t,e,a,u,i)}}}}}})}function nt(r){return z(7,r,function(n){return function(t){return function(e){return function(a){return function(u){return function(i){return function(o){return r(n,t,e,a,u,i,o)}}}}}}})}function tt(r){return z(8,r,function(n){return function(t){return function(e){return function(a){return function(u){return function(i){return function(o){return function(v){return r(n,t,e,a,u,i,o,v)}}}}}}}})}function et(r){return z(9,r,function(n){return function(t){return function(e){return function(a){return function(u){return function(i){return function(o){return function(v){return function(c){return r(n,t,e,a,u,i,o,v,c)}}}}}}}}})}function $(r,n,t){return r.a===2?r.f(n,t):r(n)(t)}function b(r,n,t,e){return r.a===3?r.f(n,t,e):r(n)(t)(e)}function R(r,n,t,e,a){return r.a===4?r.f(n,t,e,a):r(n)(t)(e)(a)}function j(r,n,t,e,a,u){return r.a===5?r.f(n,t,e,a,u):r(n)(t)(e)(a)(u)}function Jr(r,n,t,e,a,u,i){return r.a===6?r.f(n,t,e,a,u,i):r(n)(t)(e)(a)(u)(i)}function ce(r,n,t,e,a,u,i,o){return r.a===7?r.f(n,t,e,a,u,i,o):r(n)(t)(e)(a)(u)(i)(o)}function le(r,n,t,e,a,u,i,o,v){return r.a===8?r.f(n,t,e,a,u,i,o,v):r(n)(t)(e)(a)(u)(i)(o)(v)}function at(r,n){for(var t,e=[],a=Kr(r,n,0,e);a&&(t=e.pop());a=Kr(t.a,t.b,0,e));return a}function Kr(r,n,t,e){if(r===n)return!0;if(typeof r!="object"||r===null||n===null)return typeof r=="function"&&Or(5),!1;if(t>100)return e.push(B(r,n)),!0;r.$<0&&(r=rn(r),n=rn(n));for(var a in r)if(!Kr(r[a],n[a],t+1,e))return!1;return!0}f(at);f(function(r,n){return!at(r,n)});function J(r,n,t){if(typeof r!="object")return r===n?0:r<n?-1:1;if(typeof r.$=="undefined")return(t=J(r.a,n.a))||(t=J(r.b,n.b))?t:J(r.c,n.c);for(;r.b&&n.b&&!(t=J(r.a,n.a));r=r.b,n=n.b);return t||(r.b?1:n.b?-1:0)}f(function(r,n){return J(r,n)<0});f(function(r,n){return J(r,n)<1});f(function(r,n){return J(r,n)>0});f(function(r,n){return J(r,n)>=0});var se=f(function(r,n){var t=J(r,n);return t<0?pt:t?Ha:gt}),or=0;function B(r,n){return{a:r,b:n}}function Fr(r,n,t){return{a:r,b:n,c:t}}function On(r,n){var t={};for(var e in r)t[e]=r[e];for(var e in n)t[e]=n[e];return t}f(C);function C(r,n){if(typeof r=="string")return r+n;if(!r.b)return n;var t=W(r.a,n);r=r.b;for(var e=t;r.b;r=r.b)e=e.b=W(r.a,n);return t}var A={$:0};function W(r,n){return{$:1,a:r,b:n}}var me=f(W);function F(r){for(var n=A,t=r.length;t--;)n=W(r[t],n);return n}function cn(r){for(var n=[];r.b;r=r.b)n.push(r.a);return n}var _e=g(function(r,n,t){for(var e=[];n.b&&t.b;n=n.b,t=t.b)e.push($(r,n.a,t.a));return F(e)});U(function(r,n,t,e){for(var a=[];n.b&&t.b&&e.b;n=n.b,t=t.b,e=e.b)a.push(b(r,n.a,t.a,e.a));return F(a)});tr(function(r,n,t,e,a){for(var u=[];n.b&&t.b&&e.b&&a.b;n=n.b,t=t.b,e=e.b,a=a.b)u.push(R(r,n.a,t.a,e.a,a.a));return F(u)});hr(function(r,n,t,e,a,u){for(var i=[];n.b&&t.b&&e.b&&a.b&&u.b;n=n.b,t=t.b,e=e.b,a=a.b,u=u.b)i.push(j(r,n.a,t.a,e.a,a.a,u.a));return F(i)});f(function(r,n){return F(cn(n).sort(function(t,e){return J(r(t),r(e))}))});f(function(r,n){return F(cn(n).sort(function(t,e){var a=$(r,t,e);return a===gt?0:a===pt?-1:1}))});var be=[];function he(r){return r.length}var ge=g(function(r,n,t){for(var e=new Array(r),a=0;a<r;a++)e[a]=t(n+a);return e}),pe=f(function(r,n){for(var t=new Array(r),e=0;e<r&&n.b;e++)t[e]=n.a,n=n.b;return t.length=e,B(t,n)});f(function(r,n){return n[r]});g(function(r,n,t){for(var e=t.length,a=new Array(e),u=0;u<e;u++)a[u]=t[u];return a[r]=n,a});f(function(r,n){for(var t=n.length,e=new Array(t+1),a=0;a<t;a++)e[a]=n[a];return e[t]=r,e});g(function(r,n,t){for(var e=t.length,a=0;a<e;a++)n=$(r,t[a],n);return n});var Ae=g(function(r,n,t){for(var e=t.length-1;e>=0;e--)n=$(r,t[e],n);return n});f(function(r,n){for(var t=n.length,e=new Array(t),a=0;a<t;a++)e[a]=r(n[a]);return e});g(function(r,n,t){for(var e=t.length,a=new Array(e),u=0;u<e;u++)a[u]=$(r,n+u,t[u]);return a});g(function(r,n,t){return t.slice(r,n)});g(function(r,n,t){var e=n.length,a=r-e;a>t.length&&(a=t.length);for(var u=e+a,i=new Array(u),o=0;o<e;o++)i[o]=n[o];for(var o=0;o<a;o++)i[o+e]=t[o];return i});f(function(r,n){return n});f(function(r,n){return console.log(r+": "+Se()),n});function Se(r){return"<internals>"}function Or(r){throw new Error("https://github.com/elm/core/blob/1.0.0/hints/"+r+".md")}f(function(r,n){return r+n});f(function(r,n){return r-n});f(function(r,n){return r*n});f(function(r,n){return r/n});f(function(r,n){return r/n|0});f(Math.pow);f(function(r,n){return n%r});var De=f(function(r,n){var t=n%r;return r===0?Or(11):t>0&&r<0||t<0&&r>0?t+r:t});f(Math.atan2);function we(r){return r===1/0||r===-1/0}var Te=Math.ceil,Fe=Math.floor,je=Math.round,Hn=Math.log,Be=isNaN;f(function(r,n){return r&&n});f(function(r,n){return r||n});f(function(r,n){return r!==n});var Je=f(function(r,n){return r+n});function Ee(r){var n=r.charCodeAt(0);return isNaN(n)?M:Z(55296<=n&&n<=56319?B(r[0]+r[1],r.slice(2)):B(r[0],r.slice(1)))}f(function(r,n){return r+n});function Me(r){return r.length}f(function(r,n){for(var t=n.length,e=new Array(t),a=0;a<t;){var u=n.charCodeAt(a);if(55296<=u&&u<=56319){e[a]=r(n[a]+n[a+1]),a+=2;continue}e[a]=r(n[a]),a++}return e.join("")});f(function(r,n){for(var t=[],e=n.length,a=0;a<e;){var u=n[a],i=n.charCodeAt(a);a++,55296<=i&&i<=56319&&(u+=n[a],a++),r(u)&&t.push(u)}return t.join("")});function Ce(r){for(var n=r.length,t=new Array(n),e=0;e<n;){var a=r.charCodeAt(e);55296<=a&&a<=56319?(t[n-e]=r[e+1],e++,t[n-e]=r[e-1],e++):(t[n-e]=r[e],e++)}return t.join("")}g(function(r,n,t){for(var e=t.length,a=0;a<e;){var u=t[a],i=t.charCodeAt(a);a++,55296<=i&&i<=56319&&(u+=t[a],a++),n=$(r,u,n)}return n});var Ve=g(function(r,n,t){for(var e=t.length;e--;){var a=t[e],u=t.charCodeAt(e);56320<=u&&u<=57343&&(e--,a=t[e]+a),n=$(r,a,n)}return n}),Le=f(function(r,n){return n.split(r)}),Pe=f(function(r,n){return n.join(r)}),de=g(function(r,n,t){return t.slice(r,n)});f(function(r,n){for(var t=n.length;t--;){var e=n[t],a=n.charCodeAt(t);if(56320<=a&&a<=57343&&(t--,e=n[t]+e),r(e))return!0}return!1});var Oe=f(function(r,n){for(var t=n.length;t--;){var e=n[t],a=n.charCodeAt(t);if(56320<=a&&a<=57343&&(t--,e=n[t]+e),!r(e))return!1}return!0}),He=f(function(r,n){return n.indexOf(r)>-1}),Re=f(function(r,n){return n.indexOf(r)===0});f(function(r,n){return n.length>=r.length&&n.lastIndexOf(r)===n.length-r.length});var qe=f(function(r,n){var t=r.length;if(t<1)return A;for(var e=0,a=[];(e=n.indexOf(r,e))>-1;)a.push(e),e=e+t;return F(a)});function ut(r){return r+""}function Ue(r){for(var n=0,t=r.charCodeAt(0),e=t==43||t==45?1:0,a=e;a<r.length;++a){var u=r.charCodeAt(a);if(u<48||57<u)return M;n=10*n+u-48}return a==e?M:Z(t==45?-n:n)}function Ge(r){var n=r.charCodeAt(0);return 55296<=n&&n<=56319?(n-55296)*1024+r.charCodeAt(1)-56320+65536:n}function Ie(r){return r<0||1114111<r?"\uFFFD":r<=65535?String.fromCharCode(r):(r-=65536,String.fromCharCode(Math.floor(r/1024)+55296,r%1024+56320))}function We(r){return{$:0,a:r}}f(function(r,n){return{$:6,d:r,b:n}});f(function(r,n){return{$:7,e:r,b:n}});function K(r,n){return{$:9,f:r,g:n}}f(function(r,n){return{$:10,b:n,h:r}});var Ze=f(function(r,n){return K(r,[n])}),Ye=g(function(r,n,t){return K(r,[n,t])});U(function(r,n,t,e){return K(r,[n,t,e])});tr(function(r,n,t,e,a){return K(r,[n,t,e,a])});hr(function(r,n,t,e,a,u){return K(r,[n,t,e,a,u])});nt(function(r,n,t,e,a,u,i){return K(r,[n,t,e,a,u,i])});tt(function(r,n,t,e,a,u,i,o){return K(r,[n,t,e,a,u,i,o])});et(function(r,n,t,e,a,u,i,o,v){return K(r,[n,t,e,a,u,i,o,v])});f(function(r,n){try{var t=JSON.parse(n);return d(r,t)}catch(e){return Q($(pn,"This is not valid JSON! "+e.message,n))}});var Qe=f(function(r,n){return d(r,n)});function d(r,n){switch(r.$){case 2:return r.b(n);case 5:return n===null?mr(r.c):x("null",n);case 3:return jr(n)?Rn(r.b,n,F):x("a LIST",n);case 4:return jr(n)?Rn(r.b,n,ke):x("an ARRAY",n);case 6:var t=r.d;if(typeof n!="object"||n===null||!(t in n))return x("an OBJECT with a field named `"+t+"`",n);var c=d(r.b,n[t]);return G(c)?c:Q($(Yn,t,c.a));case 7:var e=r.e;if(!jr(n))return x("an ARRAY",n);if(e>=n.length)return x("a LONGER array. Need index "+e+" but only see "+n.length+" entries",n);var c=d(r.b,n[e]);return G(c)?c:Q($(St,e,c.a));case 8:if(typeof n!="object"||n===null||jr(n))return x("an OBJECT",n);var a=A;for(var u in n)if(n.hasOwnProperty(u)){var c=d(r.b,n[u]);if(!G(c))return Q($(Yn,u,c.a));a=W(B(u,c.a),a)}return mr(X(a));case 9:for(var i=r.f,o=r.g,v=0;v<o.length;v++){var c=d(o[v],n);if(!G(c))return c;i=i(c.a)}return mr(i);case 10:var c=d(r.b,n);return G(c)?d(r.h(c.a),n):c;case 11:for(var l=A,_=r.g;_.b;_=_.b){var c=d(_.a,n);if(G(c))return c;l=W(c.a,l)}return Q(Ra(X(l)));case 1:return Q($(pn,r.a,n));case 0:return mr(r.a)}}function Rn(r,n,t){for(var e=n.length,a=new Array(e),u=0;u<e;u++){var i=d(r,n[u]);if(!G(i))return Q($(St,u,i.a));a[u]=i.a}return mr(t(a))}function jr(r){return Array.isArray(r)||typeof FileList!="undefined"&&r instanceof FileList}function ke(r){return $(eu,r.length,function(n){return r[n]})}function x(r,n){return Q($(pn,"Expecting "+r,n))}function ar(r,n){if(r===n)return!0;if(r.$!==n.$)return!1;switch(r.$){case 0:case 1:return r.a===n.a;case 2:return r.b===n.b;case 5:return r.c===n.c;case 3:case 4:case 8:return ar(r.b,n.b);case 6:return r.d===n.d&&ar(r.b,n.b);case 7:return r.e===n.e&&ar(r.b,n.b);case 9:return r.f===n.f&&qn(r.g,n.g);case 10:return r.h===n.h&&ar(r.b,n.b);case 11:return qn(r.g,n.g)}}function qn(r,n){var t=r.length;if(t!==n.length)return!1;for(var e=0;e<t;e++)if(!ar(r[e],n[e]))return!1;return!0}var Xe=f(function(r,n){return JSON.stringify(n,null,r)+""});function ze(r){return r}g(function(r,n,t){return t[r]=n,t});function Y(r){return{$:0,a:r}}function Ke(r){return{$:1,a:r}}function L(r){return{$:2,b:r,c:null}}var yr=f(function(r,n){return{$:3,b:r,d:n}});f(function(r,n){return{$:4,b:r,d:n}});function ye(r){return{$:5,b:r}}var Ne=0;function Hr(r){var n={$:0,e:Ne++,f:r,g:null,h:[]};return sn(n),n}function ln(r){return L(function(n){n(Y(Hr(r)))})}function it(r,n){r.h.push(n),sn(r)}var xe=f(function(r,n){return L(function(t){it(r,n),t(Y(or))})});function ra(r){return L(function(n){var t=r.f;t.$===2&&t.c&&t.c(),r.f=null,n(Y(or))})}var kr=!1,Un=[];function sn(r){if(Un.push(r),!kr){for(kr=!0;r=Un.shift();)na(r);kr=!1}}function na(r){for(;r.f;){var n=r.f.$;if(n===0||n===1){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else if(n===2){r.f.c=r.f.b(function(t){r.f=t,sn(r)});return}else if(n===5){if(r.h.length===0)return;r.f=r.f.b(r.h.shift())}else r.g={$:n===3?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}U(function(r,n,t,e){return mn(n,e,r.be,r.bG,r.bB,function(){return function(){}})});function mn(r,n,t,e,a,u){var i=$(Qe,r,n?n.flags:void 0);G(i)||Or(2);var o={},v=t(i.a),c=v.a,l=u(m,c),_=ta(o,m);function m(s,h){var p=$(e,s,c);l(c=p.a,h),In(o,p.b,a(c))}return In(o,v.b,a(c)),_?{ports:_}:{}}var ir={};function ta(r,n){var t;for(var e in ir){var a=ir[e];a.a&&(t=t||{},t[e]=a.a(e,n)),r[e]=ea(a,n)}return t}function $t(r,n,t,e,a){return{b:r,c:n,d:t,e,f:a}}function ea(r,n){var t={g:n,h:void 0},e=r.c,a=r.d,u=r.e,i=r.f;function o(v){return $(yr,o,ye(function(c){var l=c.a;return c.$===0?b(a,t,l,v):u&&i?R(e,t,l.i,l.j,v):b(e,t,u?l.i:l.j,v)}))}return t.h=Hr($(yr,o,r.b))}var aa=f(function(r,n){return L(function(t){r.g(n),t(Y(or))})}),ua=f(function(r,n){return $(xe,r.h,{$:0,a:n})});function ot(r){return function(n){return{$:1,k:r,l:n}}}function ia(r){return{$:2,m:r}}f(function(r,n){return{$:3,n:r,o:n}});var Gn=[],Xr=!1;function In(r,n,t){if(Gn.push({p:r,q:n,r:t}),!Xr){Xr=!0;for(var e;e=Gn.shift();)$a(e.p,e.q,e.r);Xr=!1}}function $a(r,n,t){var e={};Er(!0,n,e,null),Er(!1,t,e,null);for(var a in r)it(r[a],{$:"fx",a:e[a]||{i:A,j:A}})}function Er(r,n,t,e){switch(n.$){case 1:var a=n.k,u=oa(r,a,e,n.l);t[a]=fa(r,u,t[a]);return;case 2:for(var i=n.m;i.b;i=i.b)Er(r,i.a,t,e);return;case 3:Er(r,n.o,t,{s:n.n,t:e});return}}function oa(r,n,t,e){function a(i){for(var o=t;o;o=o.t)i=o.s(i);return i}var u=r?ir[n].e:ir[n].f;return $(u,a,e)}function fa(r,n,t){return t=t||{i:A,j:A},r?t.i=W(n,t.i):t.j=W(n,t.j),t}f(function(r,n){return n});f(function(r,n){return function(t){return r(n(t))}});var Mr,nr=typeof document!="undefined"?document:{};function _n(r,n){r.appendChild(n)}U(function(r,n,t,e){var a=e.node;return a.parentNode.replaceChild(k(r,function(){}),a),{}});function Nr(r){return{$:0,a:r}}var ft=f(function(r,n){return f(function(t,e){for(var a=[],u=0;e.b;e=e.b){var i=e.a;u+=i.b||0,a.push(i)}return u+=a.length,{$:1,c:n,d:ct(t),e:a,f:r,b:u}})}),Rr=ft(void 0),va=f(function(r,n){return f(function(t,e){for(var a=[],u=0;e.b;e=e.b){var i=e.a;u+=i.b.b||0,a.push(i)}return u+=a.length,{$:2,c:n,d:ct(t),e:a,f:r,b:u}})});va(void 0);f(function(r,n){return{$:4,j:r,k:n,b:1+(n.b||0)}});function y(r,n){return{$:5,l:r,m:n,k:void 0}}f(function(r,n){return y([r,n],function(){return r(n)})});g(function(r,n,t){return y([r,n,t],function(){return $(r,n,t)})});U(function(r,n,t,e){return y([r,n,t,e],function(){return b(r,n,t,e)})});tr(function(r,n,t,e,a){return y([r,n,t,e,a],function(){return R(r,n,t,e,a)})});hr(function(r,n,t,e,a,u){return y([r,n,t,e,a,u],function(){return j(r,n,t,e,a,u)})});nt(function(r,n,t,e,a,u,i){return y([r,n,t,e,a,u,i],function(){return Jr(r,n,t,e,a,u,i)})});tt(function(r,n,t,e,a,u,i,o){return y([r,n,t,e,a,u,i,o],function(){return ce(r,n,t,e,a,u,i,o)})});et(function(r,n,t,e,a,u,i,o,v){return y([r,n,t,e,a,u,i,o,v],function(){return le(r,n,t,e,a,u,i,o,v)})});var ca=f(function(r,n){return{$:"a0",n:r,o:n}});f(function(r,n){return{$:"a1",n:r,o:n}});var la=f(function(r,n){return{$:"a2",n:r,o:n}}),w=f(function(r,n){return{$:"a3",n:r,o:n}});g(function(r,n,t){return{$:"a4",n,o:{f:r,o:t}}});function vt(r){return/^javascript:/i.test(r.replace(/\s/g,""))?"":r}f(function(r,n){return n.$==="a0"?$(ca,n.n,sa(r,n.o)):n});function sa(r,n){var t=Dn(n);return{$:n.$,a:t?b(uu,t<3?ma:_a,Vt(r),n.a):$(au,r,n.a)}}var ma=f(function(r,n){return B(r(n.a),n.b)}),_a=f(function(r,n){return{v:r(n.v),aa:n.aa,Y:n.Y}});function ct(r){for(var n={};r.b;r=r.b){var t=r.a,e=t.$,a=t.n,u=t.o;if(e==="a2"){a==="className"?Wn(n,a,u):n[a]=u;continue}var i=n[e]||(n[e]={});e==="a3"&&a==="class"?Wn(i,a,u):i[a]=u}return n}function Wn(r,n,t){var e=r[n];r[n]=e?e+" "+t:t}function k(r,n){var t=r.$;if(t===5)return k(r.k||(r.k=r.m()),n);if(t===0)return nr.createTextNode(r.a);if(t===4){for(var e=r.k,a=r.j;e.$===4;)typeof a!="object"?a=[a,e.j]:a.push(e.j),e=e.k;var u={j:a,p:n},i=k(e,u);return i.elm_event_node_ref=u,i}if(t===3){var i=r.h(r.g);return xr(i,n,r.d),i}var i=r.f?nr.createElementNS(r.f,r.c):nr.createElement(r.c);Mr&&r.c=="a"&&i.addEventListener("click",Mr(i)),xr(i,n,r.d);for(var o=r.e,v=0;v<o.length;v++)_n(i,k(t===1?o[v]:o[v].b,n));return i}function xr(r,n,t){for(var e in t){var a=t[e];e==="a1"?ba(r,a):e==="a0"?pa(r,n,a):e==="a3"?ha(r,a):e==="a4"?ga(r,a):(e!=="value"&&e!=="checked"||r[e]!==a)&&(r[e]=a)}}function ba(r,n){var t=r.style;for(var e in n)t[e]=n[e]}function ha(r,n){for(var t in n){var e=n[t];typeof e!="undefined"?r.setAttribute(t,e):r.removeAttribute(t)}}function ga(r,n){for(var t in n){var e=n[t],a=e.f,u=e.o;typeof u!="undefined"?r.setAttributeNS(a,t,u):r.removeAttributeNS(a,t)}}function pa(r,n,t){var e=r.elmFs||(r.elmFs={});for(var a in t){var u=t[a],i=e[a];if(!u){r.removeEventListener(a,i),e[a]=void 0;continue}if(i){var o=i.q;if(o.$===u.$){i.q=u;continue}r.removeEventListener(a,i)}i=Aa(n,u),r.addEventListener(a,i,bn&&{passive:Dn(u)<2}),e[a]=i}}var bn;try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){bn=!0}}))}catch{}function Aa(r,n){function t(e){var a=t.q,u=d(a.a,e);if(!!G(u)){for(var i=Dn(a),o=u.a,v=i?i<3?o.a:o.v:o,c=i==1?o.b:i==3&&o.aa,l=(c&&e.stopPropagation(),(i==2?o.b:i==3&&o.Y)&&e.preventDefault(),r),_,m;_=l.j;){if(typeof _=="function")v=_(v);else for(var m=_.length;m--;)v=_[m](v);l=l.p}l(v,c)}}return t.q=n,t}function Sa(r,n){return r.$==n.$&&ar(r.a,n.a)}function lt(r,n){var t=[];return O(r,n,t,0),t}function E(r,n,t,e){var a={$:n,r:t,s:e,t:void 0,u:void 0};return r.push(a),a}function O(r,n,t,e){if(r!==n){var a=r.$,u=n.$;if(a!==u)if(a===1&&u===2)n=Ea(n),u=1;else{E(t,0,e,n);return}switch(u){case 5:for(var i=r.l,o=n.l,v=i.length,c=v===o.length;c&&v--;)c=i[v]===o[v];if(c){n.k=r.k;return}n.k=n.m();var l=[];O(r.k,n.k,l,0),l.length>0&&E(t,1,e,l);return;case 4:for(var _=r.j,m=n.j,s=!1,h=r.k;h.$===4;)s=!0,typeof _!="object"?_=[_,h.j]:_.push(h.j),h=h.k;for(var p=n.k;p.$===4;)s=!0,typeof m!="object"?m=[m,p.j]:m.push(p.j),p=p.k;if(s&&_.length!==m.length){E(t,0,e,n);return}(s?!Da(_,m):_!==m)&&E(t,2,e,m),O(h,p,t,e+1);return;case 0:r.a!==n.a&&E(t,3,e,n.a);return;case 1:Zn(r,n,t,e,wa);return;case 2:Zn(r,n,t,e,Ta);return;case 3:if(r.h!==n.h){E(t,0,e,n);return}var S=hn(r.d,n.d);S&&E(t,4,e,S);var T=n.i(r.g,n.g);T&&E(t,5,e,T);return}}}function Da(r,n){for(var t=0;t<r.length;t++)if(r[t]!==n[t])return!1;return!0}function Zn(r,n,t,e,a){if(r.c!==n.c||r.f!==n.f){E(t,0,e,n);return}var u=hn(r.d,n.d);u&&E(t,4,e,u),a(r,n,t,e)}function hn(r,n,t){var e;for(var a in r){if(a==="a1"||a==="a0"||a==="a3"||a==="a4"){var u=hn(r[a],n[a]||{},a);u&&(e=e||{},e[a]=u);continue}if(!(a in n)){e=e||{},e[a]=t?t==="a1"?"":t==="a0"||t==="a3"?void 0:{f:r[a].f,o:void 0}:typeof r[a]=="string"?"":null;continue}var i=r[a],o=n[a];i===o&&a!=="value"&&a!=="checked"||t==="a0"&&Sa(i,o)||(e=e||{},e[a]=o)}for(var v in n)v in r||(e=e||{},e[v]=n[v]);return e}function wa(r,n,t,e){var a=r.e,u=n.e,i=a.length,o=u.length;i>o?E(t,6,e,{v:o,i:i-o}):i<o&&E(t,7,e,{v:i,e:u});for(var v=i<o?i:o,c=0;c<v;c++){var l=a[c];O(l,u[c],t,++e),e+=l.b||0}}function Ta(r,n,t,e){for(var a=[],u={},i=[],o=r.e,v=n.e,c=o.length,l=v.length,_=0,m=0,s=e;_<c&&m<l;){var h=o[_],p=v[m],S=h.a,T=p.a,D=h.b,N=p.b,fr=void 0,vr=void 0;if(S===T){s++,O(D,N,a,s),s+=D.b||0,_++,m++;continue}var wr=o[_+1],Yr=v[m+1];if(wr){var Pn=wr.a,er=wr.b;vr=T===Pn}if(Yr){var dn=Yr.a,Qr=Yr.b;fr=S===dn}if(fr&&vr){s++,O(D,Qr,a,s),cr(u,a,S,N,m,i),s+=D.b||0,s++,lr(u,a,S,er,s),s+=er.b||0,_+=2,m+=2;continue}if(fr){s++,cr(u,a,T,N,m,i),O(D,Qr,a,s),s+=D.b||0,_+=1,m+=2;continue}if(vr){s++,lr(u,a,S,D,s),s+=D.b||0,s++,O(er,N,a,s),s+=er.b||0,_+=2,m+=1;continue}if(wr&&Pn===dn){s++,lr(u,a,S,D,s),cr(u,a,T,N,m,i),s+=D.b||0,s++,O(er,Qr,a,s),s+=er.b||0,_+=2,m+=2;continue}break}for(;_<c;){s++;var h=o[_],D=h.b;lr(u,a,h.a,D,s),s+=D.b||0,_++}for(;m<l;){var Tr=Tr||[],p=v[m];cr(u,a,p.a,p.b,void 0,Tr),m++}(a.length>0||i.length>0||Tr)&&E(t,8,e,{w:a,x:i,y:Tr})}var st="_elmW6BL";function cr(r,n,t,e,a,u){var i=r[t];if(!i){i={c:0,z:e,r:a,s:void 0},u.push({r:a,A:i}),r[t]=i;return}if(i.c===1){u.push({r:a,A:i}),i.c=2;var o=[];O(i.z,e,o,i.r),i.r=a,i.s.s={w:o,A:i};return}cr(r,n,t+st,e,a,u)}function lr(r,n,t,e,a){var u=r[t];if(!u){var i=E(n,9,a,void 0);r[t]={c:1,z:e,r:a,s:i};return}if(u.c===0){u.c=2;var o=[];O(e,u.z,o,a),E(n,9,a,{w:o,A:u});return}lr(r,n,t+st,e,a)}function mt(r,n,t,e){sr(r,n,t,0,0,n.b,e)}function sr(r,n,t,e,a,u,i){for(var o=t[e],v=o.r;v===a;){var c=o.$;if(c===1)mt(r,n.k,o.s,i);else if(c===8){o.t=r,o.u=i;var l=o.s.w;l.length>0&&sr(r,n,l,0,a,u,i)}else if(c===9){o.t=r,o.u=i;var _=o.s;if(_){_.A.s=r;var l=_.w;l.length>0&&sr(r,n,l,0,a,u,i)}}else o.t=r,o.u=i;if(e++,!(o=t[e])||(v=o.r)>u)return e}var m=n.$;if(m===4){for(var s=n.k;s.$===4;)s=s.k;return sr(r,s,t,e,a+1,u,r.elm_event_node_ref)}for(var h=n.e,p=r.childNodes,S=0;S<h.length;S++){a++;var T=m===1?h[S]:h[S].b,D=a+(T.b||0);if(a<=v&&v<=D&&(e=sr(p[S],T,t,e,a,D,i),!(o=t[e])||(v=o.r)>u))return e;a=D}return e}function _t(r,n,t,e){return t.length===0?r:(mt(r,n,t,e),Cr(r,t))}function Cr(r,n){for(var t=0;t<n.length;t++){var e=n[t],a=e.t,u=Fa(a,e);a===r&&(r=u)}return r}function Fa(r,n){switch(n.$){case 0:return ja(r,n.s,n.u);case 4:return xr(r,n.u,n.s),r;case 3:return r.replaceData(0,r.length,n.s),r;case 1:return Cr(r,n.s);case 2:return r.elm_event_node_ref?r.elm_event_node_ref.j=n.s:r.elm_event_node_ref={j:n.s,p:n.u},r;case 6:for(var u=n.s,e=0;e<u.i;e++)r.removeChild(r.childNodes[u.v]);return r;case 7:for(var u=n.s,t=u.e,e=u.v,a=r.childNodes[e];e<t.length;e++)r.insertBefore(k(t[e],n.u),a);return r;case 9:var u=n.s;if(!u)return r.parentNode.removeChild(r),r;var i=u.A;return typeof i.r!="undefined"&&r.parentNode.removeChild(r),i.s=Cr(r,u.w),r;case 8:return Ba(r,n);case 5:return n.s(r);default:Or(10)}}function ja(r,n,t){var e=r.parentNode,a=k(n,t);return a.elm_event_node_ref||(a.elm_event_node_ref=r.elm_event_node_ref),e&&a!==r&&e.replaceChild(a,r),a}function Ba(r,n){var t=n.s,e=Ja(t.y,n);r=Cr(r,t.w);for(var a=t.x,u=0;u<a.length;u++){var i=a[u],o=i.A,v=o.c===2?o.s:k(o.z,n.u);r.insertBefore(v,r.childNodes[i.r])}return e&&_n(r,e),r}function Ja(r,n){if(!!r){for(var t=nr.createDocumentFragment(),e=0;e<r.length;e++){var a=r[e],u=a.A;_n(t,u.c===2?u.s:k(u.z,n.u))}return t}}function gn(r){if(r.nodeType===3)return Nr(r.textContent);if(r.nodeType!==1)return Nr("");for(var n=A,t=r.attributes,e=t.length;e--;){var a=t[e],u=a.name,i=a.value;n=W($(w,u,i),n)}for(var o=r.tagName.toLowerCase(),v=A,c=r.childNodes,e=c.length;e--;)v=W(gn(c[e]),v);return b(Rr,o,n,v)}function Ea(r){for(var n=r.e,t=n.length,e=new Array(t),a=0;a<t;a++)e[a]=n[a].b;return{$:1,c:r.c,d:r.d,e,f:r.f,b:r.b}}var Ma=U(function(r,n,t,e){return mn(n,e,r.be,r.bG,r.bB,function(a,u){var i=r.bH,o=e.node,v=gn(o);return bt(u,function(c){var l=i(c),_=lt(v,l);o=_t(o,v,_,a),v=l})})});U(function(r,n,t,e){return mn(n,e,r.be,r.bG,r.bB,function(a,u){var i=r.Z&&r.Z(a),o=r.bH,v=nr.title,c=nr.body,l=gn(c);return bt(u,function(_){Mr=i;var m=o(_),s=Rr("body")(A)(m.a$),h=lt(l,s);c=_t(c,l,h,a),l=s,Mr=0,v!==m.bF&&(nr.title=v=m.bF)})})});var Vr=typeof requestAnimationFrame!="undefined"?requestAnimationFrame:function(r){return setTimeout(r,1e3/60)};function bt(r,n){n(r);var t=0;function e(){t=t===1?0:(Vr(e),n(r),1)}return function(a,u){r=a,u?(n(r),t===2&&(t=1)):(t===0&&Vr(e),t=2)}}f(function(r,n){return $(Wr,wn,L(function(){n&&history.go(n),r()}))});f(function(r,n){return $(Wr,wn,L(function(){history.pushState({},"",n),r()}))});f(function(r,n){return $(Wr,wn,L(function(){history.replaceState({},"",n),r()}))});var Ca={addEventListener:function(){},removeEventListener:function(){}},Va=typeof window!="undefined"?window:Ca;g(function(r,n,t){return ln(L(function(e){function a(u){Hr(t(u))}return r.addEventListener(n,a,bn&&{passive:!0}),function(){r.removeEventListener(n,a)}}))});f(function(r,n){var t=d(r,n);return G(t)?Z(t.a):M});function ht(r,n){return L(function(t){Vr(function(){var e=document.getElementById(r);t(e?Y(n(e)):Ke(iu(r)))})})}function La(r){return L(function(n){Vr(function(){n(Y(r()))})})}f(function(r,n){return ht(n,function(t){return t[r](),or})});f(function(r,n){return La(function(){return Va.scroll(r,n),or})});g(function(r,n,t){return ht(r,function(e){return e.scrollLeft=n,e.scrollTop=t,or})});function Pa(r){return L(function(n){n(Y(r(Date.now())))})}var da=f(function(r,n){return L(function(t){var e=setInterval(function(){Hr(n)},r);return function(){clearInterval(e)}})});function Oa(){return L(function(r){r(Y($(gu,-new Date().getTimezoneOffset(),A)))})}f(function(r,n){return r&n});f(function(r,n){return r|n});f(function(r,n){return r^n});f(function(r,n){return n<<r});f(function(r,n){return n>>r});f(function(r,n){return n>>>r});var gt=1,Ha=2,pt=0,V=me,At=g(function(r,n,t){r:for(;;){if(t.$===-2)return n;var e=t.b,a=t.c,u=t.d,i=t.e,o=r,v=b(r,e,a,b(At,r,n,i)),c=u;r=o,n=v,t=c;continue r}}),rn=function(r){return b(At,g(function(n,t,e){return $(V,B(n,t),e)}),A,r)},Br=Ae;g(function(r,n,t){var e=t.c,a=t.d,u=f(function(i,o){if(i.$){var c=i.a;return b(Br,r,o,c)}else{var v=i.a;return b(Br,u,o,v)}});return b(Br,u,b(Br,r,n,a),e)});var Q=function(r){return{$:1,a:r}},pn=f(function(r,n){return{$:3,a:r,b:n}}),Yn=f(function(r,n){return{$:0,a:r,b:n}}),St=f(function(r,n){return{$:1,a:r,b:n}}),mr=function(r){return{$:0,a:r}},Ra=function(r){return{$:2,a:r}},Z=function(r){return{$:0,a:r}},M={$:1},qa=Oe,Ua=Xe,$r=ut,_r=f(function(r,n){return $(Pe,r,cn(n))}),An=f(function(r,n){return F($(Le,r,n))}),Dt=function(r){return $(_r,`
    `,$(An,`
`,r))},gr=g(function(r,n,t){r:for(;;)if(t.b){var e=t.a,a=t.b,u=r,i=$(r,e,n),o=a;r=u,n=i,t=o;continue r}else return n}),wt=function(r){return b(gr,f(function(n,t){return t+1}),0,r)},Ga=_e,Ia=g(function(r,n,t){r:for(;;)if(J(r,n)<1){var e=r,a=n-1,u=$(V,n,t);r=e,n=a,t=u;continue r}else return t}),Sn=f(function(r,n){return b(Ia,r,n,A)}),Wa=f(function(r,n){return b(Ga,r,$(Sn,0,wt(n)-1),n)}),pr=Ge,Tt=function(r){var n=pr(r);return 97<=n&&n<=122},Ft=function(r){var n=pr(r);return n<=90&&65<=n},Za=function(r){return Tt(r)||Ft(r)},Ya=function(r){var n=pr(r);return n<=57&&48<=n},Qa=function(r){return Tt(r)||Ft(r)||Ya(r)},X=function(r){return b(gr,V,A,r)},Ar=Ee,ka=f(function(r,n){return`

(`+($r(r+1)+(") "+Dt(Xa(n))))}),Xa=function(r){return $(za,r,A)},za=f(function(r,n){r:for(;;)switch(r.$){case 0:var t=r.a,i=r.b,e=function(){var p=Ar(t);if(p.$===1)return!1;var S=p.a,T=S.a,D=S.b;return Za(T)&&$(qa,Qa,D)}(),a=e?"."+t:"['"+(t+"']"),v=i,c=$(V,a,n);r=v,n=c;continue r;case 1:var u=r.a,i=r.b,o="["+($r(u)+"]"),v=i,c=$(V,o,n);r=v,n=c;continue r;case 2:var l=r.a;if(l.b)if(l.b.b){var _=function(){return n.b?"The Json.Decode.oneOf at json"+$(_r,"",X(n)):"Json.Decode.oneOf"}(),h=_+(" failed in the following "+($r(wt(l))+" ways:"));return $(_r,`

`,$(V,h,$(Wa,ka,l)))}else{var i=l.a,v=i,c=n;r=v,n=c;continue r}else return"Ran into a Json.Decode.oneOf with no possibilities"+function(){return n.b?" at json"+$(_r,"",X(n)):"!"}();default:var m=r.a,s=r.b,h=function(){return n.b?"Problem with the value at json"+($(_r,"",X(n))+`:

    `):`Problem with the given value:

`}();return h+(Dt($(Ua,4,s))+(`

`+m))}}),H=32,nn=U(function(r,n,t,e){return{$:0,a:r,b:n,c:t,d:e}}),tn=be,jt=Te,Bt=f(function(r,n){return Hn(n)/Hn(r)}),en=jt($(Bt,2,H)),Ka=R(nn,0,en,tn,tn),Jt=ge,ya=function(r){return{$:1,a:r}};f(function(r,n){return r(n)});f(function(r,n){return n(r)});var Et=Fe,Qn=he,Mt=f(function(r,n){return J(r,n)>0?r:n}),Na=function(r){return{$:0,a:r}},Ct=pe,xa=f(function(r,n){r:for(;;){var t=$(Ct,H,r),e=t.a,a=t.b,u=$(V,Na(e),n);if(a.b){var i=a,o=u;r=i,n=o;continue r}else return X(u)}}),ru=f(function(r,n){r:for(;;){var t=jt(n/H);if(t===1)return $(Ct,H,r).a;var e=$(xa,r,A),a=t;r=e,n=a;continue r}}),nu=f(function(r,n){if(n.b){var t=n.b*H,e=Et($(Bt,H,t-1)),a=r?X(n.e):n.e,u=$(ru,a,n.b);return R(nn,Qn(n.d)+t,$(Mt,5,e*en),u,n.d)}else return R(nn,Qn(n.d),en,tn,n.d)}),tu=tr(function(r,n,t,e,a){r:for(;;){if(n<0)return $(nu,!1,{e,b:t/H|0,d:a});var u=ya(b(Jt,H,n,r)),i=r,o=n-H,v=t,c=$(V,u,e),l=a;r=i,n=o,t=v,e=c,a=l;continue r}}),eu=f(function(r,n){if(r<=0)return Ka;var t=r%H,e=b(Jt,t,r-t,n),a=r-t-H;return j(tu,n,a,r,A,e)}),G=function(r){return!r.$},au=Ze,uu=Ye,Vt=We,Dn=function(r){switch(r.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},Lt=function(r){return r},iu=Lt,kn=hr(function(r,n,t,e,a,u){return{am:u,ar:n,ay:e,aA:t,aD:r,aE:a}}),$u=He,rr=Me,ur=de,Sr=f(function(r,n){return r<1?n:b(ur,r,rr(n),n)}),qr=qe,Ur=function(r){return r===""},Gr=f(function(r,n){return r<1?"":b(ur,0,r,n)}),Pt=Ue,Xn=tr(function(r,n,t,e,a){if(Ur(a)||$($u,"@",a))return M;var u=$(qr,":",a);if(u.b){if(u.b.b)return M;var i=u.a,o=Pt($(Sr,i+1,a));if(o.$===1)return M;var v=o;return Z(Jr(kn,r,$(Gr,i,a),v,n,t,e))}else return Z(Jr(kn,r,a,M,n,t,e))}),zn=U(function(r,n,t,e){if(Ur(e))return M;var a=$(qr,"/",e);if(a.b){var u=a.a;return j(Xn,r,$(Sr,u,e),n,t,$(Gr,u,e))}else return j(Xn,r,"/",n,t,e)}),Kn=g(function(r,n,t){if(Ur(t))return M;var e=$(qr,"?",t);if(e.b){var a=e.a;return R(zn,r,Z($(Sr,a+1,t)),n,$(Gr,a,t))}else return R(zn,r,M,n,t)});f(function(r,n){if(Ur(n))return M;var t=$(qr,"#",n);if(t.b){var e=t.a;return b(Kn,r,Z($(Sr,e+1,n)),$(Gr,e,n))}else return b(Kn,r,M,n)});var ou=Re,wn=function(r){},q=Y,fu=q(0),dt=U(function(r,n,t,e){if(e.b){var a=e.a,u=e.b;if(u.b){var i=u.a,o=u.b;if(o.b){var v=o.a,c=o.b;if(c.b){var l=c.a,_=c.b,m=t>500?b(gr,r,n,X(_)):R(dt,r,n,t+1,_);return $(r,a,$(r,i,$(r,v,$(r,l,m))))}else return $(r,a,$(r,i,$(r,v,n)))}else return $(r,a,$(r,i,n))}else return $(r,a,n)}else return n}),Tn=g(function(r,n,t){return R(dt,r,n,0,t)}),Ir=f(function(r,n){return b(Tn,f(function(t,e){return $(V,r(t),e)}),A,n)}),I=yr,Fn=f(function(r,n){return $(I,function(t){return q(r(t))},n)}),vu=g(function(r,n,t){return $(I,function(e){return $(I,function(a){return q($(r,e,a))},t)},n)}),Ot=function(r){return b(Tn,vu(V),q(A),r)},Ht=aa,cu=f(function(r,n){var t=n;return ln($(I,Ht(r),t))}),lu=g(function(r,n,t){return $(Fn,function(e){return 0},Ot($(Ir,cu(r),n)))}),su=g(function(r,n,t){return q(0)}),mu=f(function(r,n){var t=n;return $(Fn,r,t)});ir.Task=$t(fu,lu,su,mu);var _u=ot("Task"),Wr=f(function(r,n){return _u($(Fn,r,n))}),bu=Ma,hu=function(r){return{$:1,a:r}},Rt=f(function(r,n){return{$:0,a:r,b:n}}),gu=Rt,pu=Oa(),qt=Lt,Au=$(Rt,0,A),Su={K:qt(0),O:Au},Du=B(Su,$(Wr,hu,pu)),wu=function(r){return{$:0,a:r}},Ut=f(function(r,n){return{$:0,a:r,b:n}}),Gt=f(function(r,n){return{aC:n,aR:r}}),an={$:-2},Lr=an,Tu=q($(Gt,Lr,Lr)),It=se,Wt=f(function(r,n){r:for(;;){if(n.$===-2)return M;var t=n.b,e=n.c,a=n.d,u=n.e,i=$(It,r,t);switch(i){case 0:var o=r,v=a;r=o,n=v;continue r;case 1:return Z(e);default:var o=r,v=u;r=o,n=v;continue r}}}),P=tr(function(r,n,t,e,a){return{$:-1,a:r,b:n,c:t,d:e,e:a}}),yn=tr(function(r,n,t,e,a){if(a.$===-1&&!a.a){a.a;var u=a.b,i=a.c,o=a.d,v=a.e;if(e.$===-1&&!e.a){e.a;var c=e.b,l=e.c,_=e.d,m=e.e;return j(P,0,n,t,j(P,1,c,l,_,m),j(P,1,u,i,o,v))}else return j(P,r,u,i,j(P,0,n,t,e,o),v)}else if(e.$===-1&&!e.a&&e.d.$===-1&&!e.d.a){e.a;var c=e.b,l=e.c,s=e.d;s.a;var h=s.b,p=s.c,S=s.d,T=s.e,m=e.e;return j(P,0,c,l,j(P,1,h,p,S,T),j(P,1,n,t,m,a))}else return j(P,r,n,t,e,a)}),un=g(function(r,n,t){if(t.$===-2)return j(P,0,r,n,an,an);var e=t.a,a=t.b,u=t.c,i=t.d,o=t.e,v=$(It,r,a);switch(v){case 0:return j(yn,e,a,u,b(un,r,n,i),o);case 1:return j(P,e,a,n,i,o);default:return j(yn,e,a,u,i,b(un,r,n,o))}}),Pr=g(function(r,n,t){var e=b(un,r,n,t);if(e.$===-1&&!e.a){e.a;var a=e.b,u=e.c,i=e.d,o=e.e;return j(P,1,a,u,i,o)}else{var v=e;return v}}),Fu=f(function(r,n){var t=r.a,e=r.b,a=$(Wt,t,n);if(a.$===1)return b(Pr,t,F([e]),n);var u=a.a;return b(Pr,t,$(V,e,u),n)}),ju=ra,Zt=g(function(r,n,t){r:for(;;){if(t.$===-2)return n;var e=t.b,a=t.c,u=t.d,i=t.e,o=r,v=b(r,e,a,b(Zt,r,n,u)),c=i;r=o,n=v,t=c;continue r}}),Bu=hr(function(r,n,t,e,a,u){var i=g(function(l,_,m){r:for(;;){var s=m.a,h=m.b;if(s.b){var p=s.a,S=p.a,T=p.b,D=s.b;if(J(S,l)<0){var N=l,fr=_,vr=B(D,b(r,S,T,h));l=N,_=fr,m=vr;continue r}else return J(S,l)>0?B(s,b(t,l,_,h)):B(D,R(n,S,T,_,h))}else return B(s,b(t,l,_,h))}}),o=b(Zt,i,B(rn(e),u),a),v=o.a,c=o.b;return b(gr,f(function(l,_){var m=l.a,s=l.b;return b(r,m,s,_)}),c,v)}),Ju=ua,Eu=da,Mu=ln,Yt=g(function(r,n,t){if(n.b){var e=n.a,a=n.b,u=Mu($(Eu,e,$(Ju,r,e))),i=function(o){return b(Yt,r,a,b(Pr,e,o,t))};return $(I,i,u)}else return q(t)}),Cu=g(function(r,n,t){var e=t.aC,a=g(function(m,s,h){var p=h.a,S=h.b,T=h.c;return Fr(p,S,$(I,function(D){return T},ju(s)))}),u=b(gr,Fu,Lr,n),i=g(function(m,s,h){var p=h.a,S=h.b,T=h.c;return Fr($(V,m,p),S,T)}),o=U(function(m,s,h,p){var S=p.a,T=p.b,D=p.c;return Fr(S,b(Pr,m,h,T),D)}),v=Jr(Bu,i,o,a,u,e,Fr(A,Lr,q(0))),c=v.a,l=v.b,_=v.c;return $(I,function(m){return q($(Gt,u,m))},$(I,function(m){return b(Yt,r,c,l)},_))}),Vu=Pa(qt),Lu=g(function(r,n,t){var e=$(Wt,n,t.aR);if(e.$===1)return q(t);var a=e.a,u=function(i){return Ot($(Ir,function(o){return $(Ht,r,o(i))},a))};return $(I,function(i){return q(t)},$(I,u,Vu))}),Pu=g(function(r,n,t){return r(n(t))}),du=f(function(r,n){var t=n.a,e=n.b;return $(Ut,t,$(Pu,r,e))});ir.Time=$t(Tu,Cu,Lu,0,du);var Ou=ot("Time"),Hu=f(function(r,n){return Ou($(Ut,r,n))}),Ru=function(r){return $(Hu,10,wu)},qu=ia,Nn=qu(A),Uu=f(function(r,n){if(r.$){var e=r.a;return B(On(n,{O:e}),Nn)}else{var t=r.a;return B(On(n,{K:t}),Nn)}}),Gu=ze,Iu=f(function(r,n){return $(la,r,Gu(n))}),Qt=Iu("className"),Wu=Rr("div"),Dr=ft("http://www.w3.org/2000/svg"),Zu=Dr("svg"),Yu=w("viewBox"),jn=w("fill"),Bn=w("height"),Jn=Dr("rect"),kt=w("stroke"),Xt=w("stroke-width"),Qu=f(function(r,n){return b(Tn,f(function(t,e){return r(t)?$(V,t,e):e}),A,n)}),zt=Dr("g"),Kt=je,yt=w("rx"),Nt=w("ry"),En=w("transform"),Mn=w("width"),xt=w("x"),re=w("y"),ku=function(){var r=function(t){return $r(Kt(function(e){return e/60*360}(t)))},n=function(t){return $(Jn,F([Mn("2"),Bn("2"),xt("59"),re("12"),yt("1"),Nt("1"),jn("white"),En("rotate("+(r(t)+",60,60)"))]),A)};return $(zt,A,$(Ir,n,$(Qu,function(t){return!!(t%5)},$(Sn,0,59))))}(),Xu=function(){var r=function(t){return $r(Kt(function(e){return e/12*360}(t)))},n=function(t){return $(Jn,F([Mn("2"),Bn("10"),xt("59"),re("5"),yt("1"),Nt("1"),jn("white"),En("rotate("+(r(t)+",60,60)"))]),A)};return $(zt,A,$(Ir,n,$(Sn,0,11)))}(),zu=w("additive"),xn=Dr("animateTransform"),Ku=w("attributeName"),yu=w("attributeType"),Nu=w("dur"),xu=function(r){return $(w,"from",vt(r))},ri=Dr("line"),ni=w("repeatCount"),dr=function(r){return r<0?-r:r},ti=f(function(r,n){r:for(;;)if(n.b){var t=n.a,e=n.b;if(r(t))return!0;var a=r,u=e;r=a,n=u;continue r}else return!1}),ei=Ve,ai=function(r){return b(ei,V,A,r)},ui=f(function(r,n){var t=$(ti,function(e){return e!=="0"&&e!=="."},ai(n));return C(r&&t?"-":"",n)}),ne=ut,$n=Je,ii=Ie,te=function(r){var n=r.a,t=r.b;if(n==="9"){var e=Ar(t);if(e.$===1)return"01";var a=e.a;return $($n,"0",te(a))}else{var u=pr(n);return u>=48&&u<57?$($n,ii(u+1),t):"0"}},$i=we,oi=Be,on=f(function(r,n){if(n.$)return M;var t=n.a;return Z(r(t))}),Cn=function(r){return $($n,r,"")},ee=g(function(r,n,t){return r<=0?t:b(ee,r>>1,C(n,n),r&1?C(t,n):t)}),br=f(function(r,n){return b(ee,r,n,"")}),fn=g(function(r,n,t){return C(t,$(br,r-rr(t),Cn(n)))}),rt=Ce,ae=function(r){var n=$(An,".",r);if(n.b)if(n.b.b){var t=n.a,e=n.b,a=e.a;return B(t,a)}else{var t=n.a;return B(t,"0")}else return B("0","0")},fi=f(function(r,n){var t=n.a,e=n.b;return B(r(t),e)}),vn=f(function(r,n){if(n.$)return r;var t=n.a;return t}),vi=function(r){var n=$(An,"e",ne(dr(r)));if(n.b)if(n.b.b){var t=n.a,e=n.b,a=e.a,u=$(vn,0,Pt($(ou,"+",a)?$(Sr,1,a):a)),i=ae(t),o=i.a,v=i.b,c=C(o,v),l=u<0?$(vn,"0",$(on,function(_){var m=_.a,s=_.b;return m+("."+s)},$(on,fi(Cn),Ar(C($(br,dr(u),"0"),c))))):b(fn,u+1,"0",c);return C(r<0?"-":"",l)}else{var t=n.a;return C(r<0?"-":"",t)}else return""},ci=g(function(r,n,t){if($i(t)||oi(t))return ne(t);var e=t<0,a=ae(vi(dr(t))),u=a.a,i=a.b,o=rr(u)+n,v=C($(br,-o+1,"0"),b(fn,o,"0",C(u,i))),c=rr(v),l=$(Mt,1,o),_=$(r,e,b(ur,l,c,v)),m=b(ur,0,l,v),s=_?rt($(vn,"1",$(on,te,Ar(rt(m))))):m,h=rr(s),p=s==="0"?s:n<=0?C(s,$(br,dr(n),"0")):J(n,rr(i))<0?b(ur,0,h-n,s)+("."+b(ur,h-n,h,s)):C(u+".",b(fn,n,"0",i));return $(ui,e,p)}),li=ci(f(function(r,n){var t=Ar(n);if(t.$===1)return!1;if(t.a.a==="5")return t.a.b===""?(t.a,!r):(t.a,!0);var e=t.a,a=e.a;return function(u){return u>53&&r||u>=53&&!r}(pr(a))})),si=function(r){return $(w,"to",vt(r))},Vn=f(function(r,n){return Et(r/n)}),Zr=De,Ln=function(r){var n=r;return n},mi=g(function(r,n,t){r:for(;;)if(t.b){var e=t.a,a=t.b;if(J(e._,n)<0)return n+e.ax;var u=r,i=n,o=a;r=u,n=i,t=o;continue r}else return n+r}),ue=f(function(r,n){var t=r.a,e=r.b;return b(mi,t,$(Vn,Ln(n),6e4),e)}),ie=f(function(r,n){return $(Zr,24,$(Vn,$(ue,r,n),60))}),$e=f(function(r,n){return $(Zr,1e3,Ln(n))}),oe=f(function(r,n){return $(Zr,60,$(ue,r,n))}),fe=f(function(r,n){return $(Zr,60,$(Vn,Ln(n),1e3))}),_i=w("type"),bi=w("x1"),hi=w("x2"),gi=w("y1"),pi=w("y2"),zr=g(function(r,n,t){var e=function(){switch(r){case 0:return"black";case 1:return"black";default:return"red"}}(),a=function(){switch(r){case 0:return"5";case 1:return"3";default:return"1"}}(),u=function(){switch(r){case 0:return"12%";case 1:return"8%";default:return"2%"}}(),i=function(){return r===2?$(xn,F([Ku("transform"),yu("XML"),_i("rotate"),xu("0 60 60"),si("0 60 60"),Nu("1s"),ni("indefinite"),zu("sum")]),A):$(xn,A,A)}(),o=function(c){switch(r){case 0:return c/12*360;case 1:return c/60*360;default:return c/1e3/60*360}},v=function(c){return $(li,2,c)}(o($(function(){switch(r){case 0:return ie;case 1:return oe;default:return f(function(c,l){return 1e3*$(fe,c,l)+$($e,c,l)})}}(),n,t)));return $(ri,F([bi("50%"),hi("50%"),gi("50%"),pi(u),En("rotate("+(v+",60,60)")),kt(e),Xt(a)]),F([i]))}),Ai=f(function(r,n){return F([$(Jn,F([Mn("100%"),Bn("100%"),jn("transparent"),kt("black"),Xt("1")]),A),Xu,ku,b(zr,0,r,n),b(zr,1,r,n),b(zr,2,r,n)])}),Si=f(function(r,n){return $(Zu,F([Yu("0 0 120 120")]),$(Ai,r,n))}),Di=g(function(r,n,t){return C($(br,r-rr(t),Cn(n)),t)}),wi=Rr("span"),Ti=Nr,Fi=Ti,ji=f(function(r,n){var t=function(o){return b(Di,2,"0",$r($(o,r,n)))},e=t(fe),a=t(oe),u=t($e),i=t(ie);return $(wi,F([Qt("digital-time")]),F([Fi(i+(":"+(a+(":"+(e+("."+u))))))]))}),Bi=function(r){return $(Wu,F([Qt("container")]),F([$(Si,r.O,r.K),$(ji,r.O,r.K)]))},Ji=bu({be:function(r){return Du},bB:Ru,bG:Uu,bH:Bi});const Ei={Main:{init:Ji(Vt(0))(0)}},Mi=document.querySelector("#app div");Ei.Main.init({node:Mi});