FROM archlinux/base

RUN pacman -Syu --noconfirm
RUN pacman -S --needed --noconfirm git cmake rhash base-devel gcc go fakeroot pkg-config yajl expac

RUN useradd -m fimex-build -g wheel
ADD fimex.sudo /etc/sudoers.d/fimex
USER fimex-build

WORKDIR /tmp

ENV PATH="/usr/bin/core_perl:${PATH}"

RUN git clone https://aur.archlinux.org/yay-git.git
WORKDIR /tmp/yay-git
RUN makepkg -s
RUN sudo pacman -U --noconfirm *.zst

ENV EDITOR="/usr/sbin/sed"
ENV CMAKE_Fortran_FLAGS=-fallow-argument-mismatch
RUN yay -S --needed --noconfirm udunits proj boost openjpeg libpng python2 pybind11 netcdf libaec jasper gcc-fortran python python-numpy

WORKDIR /tmp
RUN git clone https://aur.archlinux.org/eccodes
WORKDIR /tmp/eccodes
RUN sed -i '26i\ \ \ \ -DCMAKE_Fortran_FLAGS=-fallow-argument-mismatch \\' PKGBUILD
RUN makepkg
RUN sudo pacman -U --noconfirm *.zst

WORKDIR /home/fimex-build

RUN git clone https://github.com/metno/fimex.git
RUN mkdir -p fimex/third_party
WORKDIR /home/fimex-build/fimex/third_party
RUN git clone https://github.com/HowardHinnant/date.git
RUN git clone https://github.com/metno/mi-cpptest.git
RUN git clone https://github.com/metno/mi-programoptions.git
WORKDIR /home/fimex-build
RUN mkdir fimex-build
RUN cd fimex-build
RUN cmake -DENABLE_ECCODES=YES -DCMAKE_CXX_FLAGS=-w /home/fimex-build/fimex
RUN make -j$(grep processor /proc/cpuinfo | wc -l)
RUN make test
RUN sudo make install
