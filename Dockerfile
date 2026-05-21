FROM python:3.11-slim

ARG QUARTO_VERSION=1.5.57

ENV MPLBACKEND=Agg \
    PIP_NO_CACHE_DIR=1 \
    PYTHONDONTWRITEBYTECODE=1 \
    PYTHONUNBUFFERED=1 \
    QUARTO_PYTHON=/usr/local/bin/python

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        ca-certificates \
        curl \
        gfortran \
        libbz2-dev \
        liblzma-dev \
        libopenblas-dev \
        libxml2-dev \
        libxslt1-dev \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Quarto CLI — required to render report/report.qmd and report/slides.qmd.
RUN set -eux; \
    arch="$(dpkg --print-architecture)"; \
    case "$arch" in \
        amd64) quarto_arch=linux-amd64 ;; \
        arm64) quarto_arch=linux-arm64 ;; \
        *) echo "Unsupported architecture for Quarto: $arch" >&2; exit 1 ;; \
    esac; \
    url="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-${quarto_arch}.deb"; \
    curl -fsSL "$url" -o /tmp/quarto.deb; \
    apt-get update; \
    apt-get install -y --no-install-recommends /tmp/quarto.deb; \
    rm -f /tmp/quarto.deb; \
    rm -rf /var/lib/apt/lists/*; \
    quarto --version

COPY requirements.txt pyproject.toml README.md ./
COPY src ./src

RUN python -m pip install --upgrade pip setuptools wheel \
    && python -m pip install -r requirements.txt \
    && python -m pip install --no-deps -e . \
    && python -m ipykernel install --name python3 --display-name "Python 3" --sys-prefix

COPY . .

CMD ["python", "scripts/generate_report.py"]
