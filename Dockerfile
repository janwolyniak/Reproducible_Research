FROM python:3.11-slim AS builder

ENV PIP_NO_CACHE_DIR=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        gfortran \
        libbz2-dev \
        liblzma-dev \
        libopenblas-dev \
        libxml2-dev \
        libxslt1-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt pyproject.toml README.md ./
COPY src ./src

RUN python -m pip install --upgrade pip setuptools wheel \
    && python -m pip install --prefix=/install --ignore-installed -r requirements.txt \
    && python -m pip install --prefix=/install --no-deps .


FROM python:3.11-slim

ARG QUARTO_VERSION=1.5.57

ENV MPLBACKEND=Agg \
    PIP_NO_CACHE_DIR=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    QUARTO_PYTHON=/usr/local/bin/python

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates curl \
    && arch="$(dpkg --print-architecture)" \
    && case "$arch" in \
        amd64) quarto_arch=linux-amd64 ;; \
        arm64) quarto_arch=linux-arm64 ;; \
        *) echo "Unsupported architecture for Quarto: $arch" >&2; exit 1 ;; \
    esac \
    && url="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-${quarto_arch}.deb" \
    && curl -fsSL "$url" -o /tmp/quarto.deb \
    && apt-get install -y --no-install-recommends /tmp/quarto.deb \
    && rm -f /tmp/quarto.deb \
    && apt-get purge -y --auto-remove curl \
    && rm -rf /var/lib/apt/lists/* \
    && quarto --version

COPY --from=builder /install /usr/local

RUN python -m ipykernel install --name python3 --display-name "Python 3" --sys-prefix

COPY . .

CMD ["python", "scripts/generate_report.py"]
