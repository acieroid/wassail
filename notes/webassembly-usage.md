This is a structured dump of resources about WebAssembly.

More info could still be integrated, a lot more exists, as can be seen in [this list](https://github.com/mbasso/awesome-wasm).

Support of WebAssembly
===========================
Browsers
---------
All major browser support WebAssembly: Wikipedia says "As of May 2020, 91.66% of installed browsers (91.65% of desktop browsers and 93.32% of mobile browsers) support WebAssembly"

Programming languages
-----------------------
Wikipedia: "There have been around 40 programming languages reported to support Wasm as a compilation target"

A list of the languages supported is available [here](https://github.com/appcypher/awesome-wasm-langs).

Embedding outside of the browsers
--------------------------------------
See [Non-web Embeddings](https://webassembly.org/docs/non-web/) on the WebAssembly doc:

> While WebAssembly is designed to run on the Web, it is also desirable for it to be able to execute well in other environments, including everything from minimal shells for testing to full-blown application environments e.g. on servers in datacenters, on IoT devices, or mobile/desktop apps. It may even be desirable to execute WebAssembly embedded within larger programs.

Interpreters and runtimes
--------------
  - [Wasm3](https://github.com/wasm3/wasm3) is a fast interpreter written in C, targetting microcontrollers
  - [Wasmer](https://github.com/wasmerio/wasmer) to run standalone applications on the desktop
  - [Lucet](https://github.com/bytecodealliance/lucet) is a runtime designed to execute potentially unsafe code within other apps (focus on sandboxing)
  - [Wasmtime](https://github.com/bytecodealliance/wasmtime), a JIT runtime for WebAssembly

Frameworks
------------
  - [Blazor](https://github.com/AdrienTorris/awesome-blazor), a .NET framework to build client apps in C# and compile them to WebAssembly
  - [Uno](https://github.com/unoplatform/uno), a C# framework to build client apps
  - [Vecty](https://github.com/gopherjs/vecty), a framework for "building responsive and dynamic web frontends in Go", supporting compilation to WebAssembly
  - [Embly](https://github.com/embly/embly), a framework for serverless applications in WebAssembly

Usages of WebAssembly
=========================

Case studies
-------------
  - The developers of fastq.bio explain [How We Used WebAssembly To Speed Up Our Web App By 20X (Case Study)](https://www.smashingmagazine.com/2019/04/webassembly-speed-web-app/).
  - JP Morgan uses [Perspective](https://github.com/finos/perspective), a visualization tool developped with WebAssembly.
  - Ebay has a [use case](https://medium.com/ebaytech/webassembly-at-ebay-a-real-world-use-case-ef888f38b537)

Porting existing applications
-----------
A major use case of WebAssembly to port existing applications (written in C or others) to the Web. The typical example of this is [QuakeJS](http://www.quakejs.com/)

Getting extra speed for existing applications
-------------------------------
See [What makes WebAssembly fast](https://hacks.mozilla.org/2017/02/what-makes-webassembly-fast/).

New web applications
----------------------
The speed boost allows for more types of applications in the browser: some had constraints in terms of latency/speed, or simply were too slow: image/video manipulation, sound editing, AI, VR, 3D rendering, CAD. See the list of [use cases](https://webassembly.org/docs/use-cases/) on the WebAssembly doc.

Malwares
-------------------
Wikipedia:
> WebAssembly has been criticized for allowing greater ease of hiding the evidence for malware writers, scammers and phishing attackers; WebAssembly is only present on the user's machine in its compiled form, which "[makes malware] detection difficult".[54] The speed and concealability of WebAssembly have led to its use in hidden crypto mining on the website visitor's device

Obfuscation
---------------------
Wikipedia:
> The ability to effectively obfuscate large amounts of code can also be used to disable ad blocking and privacy tool that prevent web tracking like Privacy Badger.

Smart contracts
-----------------
From [Manticore](https://blog.trailofbits.com/2020/01/31/symbolically-executing-webassembly-in-manticore/).
> WASM is also poised to have a positive impact on our Ethereum smart contract analysis work. As part of the Ethereum 2.0 improvements, the Ethereum foundation plans to replace the Ethereum Virtual Machine (EVM) language with Ethereum-flavored WebAssembly (EWASM). EWASM will look somewhat different from regular WASM, but we think that having some experience developing WASM tools will make it easy to upgrade our existing EVM tools when the transition does take place.

They specifically talk about [ewasm](https://github.com/ewasm), the Ethereum VM ported to WebAssembly.

Also: [SSVM](https://github.com/second-state/SSVM), among other applications, is used as a smart contract runtime engine.

Server-side
-------------
Node supports WebAssembly

Microcontrollers
--------------------
  - The [Wasm3](https://github.com/wasm3/wasm3) interpreter is designed to run on microcontrollers
  - [Nebulet](https://github.com/nebulet/nebulet) is a microkernel implemented that implements a WebAssembly usermode (dead project)
  - [Wasmachine](https://github.com/piranna/wasmachine), WebAssembly for FPGAs

Distributed applications
---------------------------
  - [Lumen](https://github.com/lumen/lumen) is a runtime for Elixir applications
  - [waSCC](https://wascc.dev/) is a scalable host runtime with actors
Books
=======
There are already books on WebAssembly
  - [Level Up with WebAssembly](https://levelupwasm.com/) focuses on building "real" apps with WebAssembly.
