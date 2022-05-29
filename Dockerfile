FROM mcr.microsoft.com/dotnet/sdk:6.0-bullseye-slim AS build

RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs

WORKDIR /source

COPY *.sln .
COPY src/ElectricLemur.Muscadine.Site/*.fsproj ./src/ElectricLemur.Muscadine.Site/
RUN dotnet restore

COPY src/ElectricLemur.Muscadine.Site/. ./src/ElectricLemur.Muscadine.Site/
WORKDIR /source/src/ElectricLemur.Muscadine.Site
RUN dotnet publish -c release -o /app --no-restore

FROM mcr.microsoft.com/dotnet/aspnet:6.0-bullseye-slim
WORKDIR /app
COPY --from=build /app ./
COPY src/ElectricLemur.Muscadine.Site/WebRoot/. ./WebRoot/

EXPOSE 80

ENTRYPOINT ["dotnet", "ElectricLemur.Muscadine.Site.App.dll"]