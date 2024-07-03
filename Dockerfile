FROM archlinux

ENV OPAMYES=1
ENV OCAML_VERSION=3.07

RUN pacman -Sy --noconfirm \
    make git diffutils patch \
    gcc13 \
    opam \
    libx11

# opam seems to ignore $CC, so we have to do this...
RUN ln -s /usr/bin/gcc-13 /usr/bin/gcc
RUN ln -s /usr/bin/gcc-13 /usr/bin/cc

RUN opam init --disable-sandboxing

RUN opam switch create ${OCAML_VERSION}

WORKDIR /app

COPY . .

# RUN eval $(opam env --switch=${OCAML_VERSION})
# ^ doesn't work in here, so instead we execute whatever it evals to:
ENV OPAM_SWITCH_PREFIX='/root/.opam/3.07' 
ENV CAML_LD_LIBRARY_PATH='/root/.opam/3.07/lib/stublibs:/root/.opam/3.07/lib/ocaml/stublibs:/root/.opam/3.07/lib/ocaml' 
ENV OCAML_TOPLEVEL_PATH='/root/.opam/3.07/lib/toplevel' 
ENV MANPATH=':/root/.opam/3.07/man' 
# and add /root/bin to PATH as well
ENV PATH='/root/bin/:/root/.opam/3.07/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin' 

RUN make install

CMD ["bash"]
