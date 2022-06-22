![Docker Cloud Automated build](https://img.shields.io/docker/cloud/automated/lcolling/odp-obaidi)
![Docker Cloud Build Status](https://img.shields.io/docker/cloud/build/lcolling/odp-obaidi)

![autobuild](https://github.com/ljcolling/odp-obaidi/workflows/autobuild/badge.svg)

Open data project repo for Obaidi et al (2019)

The built document can be view [here](http://git.colling.net.nz/odp-obaidi/)


## Manual instructions

To build this document manually, run the following steps

1. Download the data. 

```bash
make download 
```

2. Run the analysis 

```bash
make analysis
```
    
3. Compile the document

```bash
make document
```

## Dependencies 

[Docker desktop](https://www.docker.com/products/docker-desktop/)
