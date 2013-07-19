wget http://www.phosphosite.org/downloads/Acetylation_site_dataset.gz
wget http://www.phosphosite.org/downloads/Kinase_Substrate_Dataset.gz
wget http://www.phosphosite.org/downloads/Methylation_site_dataset.gz
wget http://www.phosphosite.org/downloads/O-GlcNAc_site_dataset.gz
wget http://www.phosphosite.org/downloads/Phosphorylation_site_dataset.gz
wget http://www.phosphosite.org/downloads/Sumoylation_site_dataset.gz
wget http://www.phosphosite.org/downloads/Ubiquitination_site_dataset.gz
gzip -d Acetylation_site_dataset.gz
gzip -d Kinase_Substrate_Dataset.gz
gzip -d Methylation_site_dataset.gz
gzip -d O-GlcNAc_site_dataset.gz
gzip -d Phosphorylation_site_dataset.gz
gzip -d Sumoylation_site_dataset.gz
gzip -d Ubiquitination_site_dataset.gz
sed -i '1,4d' Acetylation_site_dataset
sed -i '1,4d' Kinase_Substrate_Dataset
sed -i '1,4d' Methylation_site_dataset
sed -i '1,4d' O-GlcNAc_site_dataset
sed -i '1,4d' Phosphorylation_site_dataset
sed -i '1,4d' Sumoylation_site_dataset
sed -i '1,4d' Ubiquitination_site_dataset
cat Acetylation_site_dataset Methylation_site_dataset O-GlcNAc_site_dataset Phosphorylation_site_dataset Sumoylation_site_dataset Ubiquitination_site_dataset > phosphosite_residue

