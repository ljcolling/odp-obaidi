FROM lcolling/r-verse-base 


RUN install2.r --error \
    --deps TRUE \
    here \
    foreign \
    metafor 

RUN install2.r --error psych

WORKDIR $HOME/rstudio