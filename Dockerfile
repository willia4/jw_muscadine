FROM mcr.microsoft.com/dotnet/sdk:8.0-bookworm-slim AS build

WORKDIR /source

COPY *.sln .
COPY src/ElectricLemur.Muscadine.Site/*.fsproj ./src/ElectricLemur.Muscadine.Site/
RUN dotnet restore

COPY src/ElectricLemur.Muscadine.Site/. ./src/ElectricLemur.Muscadine.Site/
WORKDIR /source/src/ElectricLemur.Muscadine.Site
RUN dotnet publish -c release -o /app --no-restore

FROM mcr.microsoft.com/dotnet/aspnet:8.0-bookworm-slim
WORKDIR /app
COPY --from=build /app ./

EXPOSE 80

ENTRYPOINT ["dotnet", "ElectricLemur.Muscadine.Site.App.dll"]